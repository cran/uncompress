
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

static R_INLINE unsigned long read_bits(const unsigned char** start, const unsigned char* end, unsigned char* pos, int num_bits) {
  unsigned long ret = 0;
  unsigned char shift = 0;
  unsigned char rpos = *pos;
  while( num_bits > 0 && *start < end ) {
    unsigned char can_read = 8-rpos;
    unsigned char bits_read = num_bits > can_read ? can_read : num_bits;
    unsigned char temp = ((**start)>>rpos)&((1<<bits_read)-1);
    ret |= ((unsigned long)temp)<<shift;
    rpos += bits_read;
    shift += bits_read;
    num_bits -= bits_read;
    if( rpos == 8 ) {
      ++(*start);
      rpos = 0;
    }
  }
  *pos = rpos;
  return ret;
}

#define POOL_BLOCK_SIZE 16384
#define POOL_ARGS char*** pool, unsigned long* pools, unsigned long* pool_used
#define POOL_PASS pool, pools, pool_used
#define POOL_PASS2 &pool, &pools, &pool_used
static R_INLINE char* init_pool(POOL_ARGS) {
  pool[0] = (char**)NULL;
  pools[0] = 0;
  pool_used[0] = POOL_BLOCK_SIZE;
}
static R_INLINE char* free_pool(POOL_ARGS) {
  unsigned long i;
  for( i = 0; i < pools[0]; ++i ) {
    free(pool[0][i]);
  }
  free(pool[0]);
  init_pool(POOL_PASS);
}
static R_INLINE char* allocate_string(unsigned long len, POOL_ARGS) {
  if( pool_used[0] + len > POOL_BLOCK_SIZE ) {
    ++pools[0];
    pool[0] = realloc(pool[0], sizeof(char*)*pools[0]);
    pool[0][pools[0]-1] = (char*)malloc(POOL_BLOCK_SIZE);
    pool_used[0] = 0;
  }
  char* ret = pool[0][pools[0]-1] + pool_used[0];
  pool_used[0] += len;
  return ret;
}

static R_INLINE unsigned char** init_table(unsigned char block_mode, char** table_cache, unsigned long* table_size, unsigned long* table_alloc) {
  int i;
  unsigned char** ret;

  *table_alloc = 1024;
  *table_size = 256 + (block_mode ? 1 : 0);
  *table_cache = (char*)malloc(768);
  ret = malloc((*table_alloc)*sizeof(unsigned char*));
  for( i = 0; i < 256; ++i ) {
    ret[i] = (unsigned char*)(*table_cache + i*3);
    ret[i][0] = 1;
    ret[i][1] = 0;
    ret[i][2] = (unsigned char)i;
  }
  memset(ret+256, 0, 768*sizeof(unsigned char*));

  return ret;
}
static R_INLINE void reinit_table(unsigned char*** table, unsigned char block_mode, unsigned long* table_size, unsigned long* table_alloc, POOL_ARGS) {
  *table_size = 256 + (block_mode ? 1 : 0);
  free_pool(POOL_PASS);
}
static R_INLINE void free_table(unsigned char** table, char** table_cache, unsigned long table_alloc, POOL_ARGS) {
  int i;

  free_pool(POOL_PASS);
//  for( i = 0; i < table_alloc; ++i ) {
//    free(table[i]);
//  }
  free(*table_cache);
  free(table);
}
static R_INLINE void add_table_entry(unsigned char*** table, unsigned long* table_size, unsigned long* table_alloc, unsigned char* str, unsigned short str_len, unsigned char chr, POOL_ARGS) {
  if( *table_size == *table_alloc ) {
    *table_alloc += 1024;
    *table = (unsigned char**)realloc(*table, (*table_alloc)*sizeof(unsigned char*));
    memset((*table) + *table_alloc - 1024, 0, 1024*sizeof(unsigned char*));
  }
  (*table)[*table_size] = (unsigned char*)allocate_string(str_len+3, POOL_PASS);
  (*table)[*table_size][0] = (str_len+1)&255;
  (*table)[*table_size][1] = (str_len+1)>>8;
  memcpy((*table)[*table_size]+2, str, str_len);
  (*table)[*table_size][2+str_len] = chr;
  *table_size += 1;
}

static R_INLINE void append_ret(unsigned char** ret, unsigned long* ret_size, unsigned long* ret_alloc, unsigned char* str, unsigned short len) {
  if( *ret_size + len > *ret_alloc ) {
    *ret_alloc += 4096;
    *ret = realloc(*ret, *ret_alloc);
  }
  memcpy((*ret)+*ret_size, str, len);
  *ret_size += len;
}

SEXP R_uncompress(const SEXP data) {
  const unsigned char* init_start, * start, * end;
  SEXP ret_data;

  unsigned char pos = 0;
  unsigned char info, block_mode, max_bits, table_bits;
  unsigned long curstr_alloc, laststr_alloc, cachestr_alloc;
  unsigned char** table;
  char* table_cache;
  unsigned char* ret, * laststr, * curstr, * cachestr;
  unsigned long table_size, table_alloc, ret_size, ret_alloc, max_entries, curstr_len, laststr_len;

  char** pool;
  unsigned long pools;
  unsigned long pool_used;
  init_pool(POOL_PASS2);

  if( TYPEOF(data) != RAWSXP ) {
    error_return("uncompress() accepts only a single RAW vector as its argument");
  }
  init_start = start = RAW_POINTER(data);
  end = start + GET_LENGTH(data);

  if( read_bits(&start, end, &pos, 8) != 31 || read_bits(&start, end, &pos, 8) != 157 ) {
    /* not valid data to uncompress */
    error_return("data passed to uncompress() does not appear to be compressed with \"compress\"");
  }
  info = read_bits(&start, end, &pos, 8);
  block_mode = info>>7;
  max_bits = info&31;
  table = init_table(block_mode, &table_cache, &table_size, &table_alloc);
  table_bits = 9;
  ret = 0;
  ret_size = 0;
  ret_alloc = 0;
  max_entries = 1<<max_bits;

  laststr = 0;
  laststr_alloc = 0;
  laststr_len = 0;
  curstr = 0;
  curstr_alloc = 0;
  curstr_len = 0;
  cachestr = 0;
  cachestr_alloc = 0;
  while(start < end) {
    unsigned long code = read_bits(&start, end, &pos, table_bits);
    if( start > end || (start == end && pos != 0) )
      break; /* end of data */
    if( block_mode && code == 256 ) {
      reinit_table(&table, block_mode, &table_size, &table_alloc, POOL_PASS2);
      table_bits = 9;
      if( laststr_alloc ) {
        if( !cachestr ) {
          cachestr = laststr;
          cachestr_alloc = laststr_alloc;
        } else {
          free(laststr);
        }
      }
      laststr = 0;
      laststr_alloc = 0;
      laststr_len = 0;
      start = init_start + (((((start - init_start)+12)>>4)<<4)+3);
    } else {
      if( code > table_size ) {
        /* invalid compressed data */
        free_table(table, &table_cache, table_alloc, POOL_PASS2);
        free(ret);
        if( curstr_alloc )
          free(curstr);
        if( laststr_alloc )
          free(laststr);
        error_return("corrupt compressed data detected in uncompress() [code value outside of table]");
      }
      if( code == table_size ) {
        unsigned char* temp = curstr;

        if( !laststr ) {
          /* invalid compressed data */
          free_table(table, &table_cache, table_alloc, POOL_PASS2);
          free(ret);
          if( curstr_alloc )
            free(curstr);
          if( laststr_alloc )
            free(laststr);
          error_return("corrupt compressed data detected in uncompress() [repeat code issued without prior token]");
        }

        ++curstr_len;
        if( curstr_alloc ) {
          if( curstr_len > curstr_alloc ) {
            curstr_alloc <<= 1;
            curstr = realloc(curstr, curstr_alloc);
          }
          if( laststr == temp )
            laststr = curstr;
        } else {
          if( curstr_len <= cachestr_alloc ) {
            curstr_alloc = cachestr_alloc;
            curstr = cachestr;
            cachestr = 0;
            cachestr_alloc = 0;
          } else {
            curstr_alloc = curstr_len<<1;
            if( curstr_alloc < 16 )
              curstr_alloc = 16;
            curstr = malloc(curstr_alloc);
          }
          memcpy(curstr, temp, curstr_len);
        }
        curstr[curstr_len-1] = laststr[0];
      } else {
        curstr_len = table[code][0] | (((unsigned short)table[code][1])<<8);
        if( curstr_alloc ) {
          if( laststr == curstr ) {
            laststr_alloc = curstr_alloc;
          } else {
            if( !cachestr ) {
              cachestr = curstr;
              cachestr_alloc = curstr_alloc;
            } else {
              free(curstr);
            }
          }
        }
        curstr = table[code]+2;
        curstr_alloc = 0;
      }

      append_ret(&ret, &ret_size, &ret_alloc, curstr, curstr_len);

      if( table_size < max_entries && laststr ) {
        add_table_entry(&table, &table_size, &table_alloc, laststr, laststr_len, curstr[0], POOL_PASS2);
        if( table_size == (1<<table_bits) && table_bits != max_bits )
          ++table_bits;
      }

      if( laststr_alloc ) {
        if( !cachestr ) {
          cachestr = laststr;
          cachestr_alloc = laststr_alloc;
        } else {
          free(laststr);
        }
        laststr_alloc = 0;
      }
      laststr = curstr;
      laststr_len = curstr_len;
    }
  }

  if( curstr_alloc )
    free(curstr);
  if( laststr_alloc )
    free(laststr);
  free_table(table, &table_cache, table_alloc, POOL_PASS2);

  ret_data = NEW_RAW(ret_size);
  memcpy(RAW_POINTER(ret_data), ret, ret_size);
  return ret_data;
}

static R_INLINE void R_rawToLines_makeStr(SEXP strs, int index, const unsigned char* last, const unsigned char* cur) {
  SEXP str;
  if( cur-last ) {
    SET_STRING_ELT(strs, index, mkCharLen(last, cur-last)); 
  }
}

SEXP R_rawToLines(const SEXP data, const SEXP start_line_sexp, const SEXP line_count_sexp) {
  const unsigned char* start, * end, * last;
  unsigned long num_lines, i, start_line, line_count;
  SEXP ret_data;

  if( TYPEOF(data) != RAWSXP || (TYPEOF(start_line_sexp) != INTSXP && TYPEOF(start_line_sexp) != REALSXP) || (TYPEOF(line_count_sexp) != INTSXP && TYPEOF(line_count_sexp) != REALSXP) ) {
    error_return("rawToLines() accepts only a single RAW vector followed by two integers as its arguments");
  }
  start = RAW_POINTER(data);
  end = start + GET_LENGTH(data);
  start_line = TYPEOF(start_line_sexp) != INTSXP ? (unsigned long)*REAL(start_line_sexp) : *INTEGER(start_line_sexp);
  line_count = TYPEOF(line_count_sexp) != INTSXP ? (unsigned long)*REAL(line_count_sexp) : *INTEGER(line_count_sexp);

  num_lines = 1;
  while( start < end ) {
    if( start[0] == '\n' ) {
      ++num_lines;
    } else if( start < end-1 && start[0] == '\r' && start[1] == '\n' ) {
      ++num_lines;
      ++start;
    }
    ++start;
    if( num_lines >= start_line+line_count )
      break;
  }
  if( start_line > num_lines ) {
    char buf[256];
    sprintf(buf, "rawToLines() called with a start line of %ld but there are only %ld lines in the data", start_line, num_lines);
    error_return(buf);
  }

  start = RAW_POINTER(data);
  ret_data = Rf_allocVector(STRSXP, num_lines-start_line);
  R_PreserveObject(ret_data);
  i = 0;
  last = start;
  while( start < end ) {
    if( start[0] == '\n' ) {
      if( i >= start_line )
        R_rawToLines_makeStr(ret_data, i-start_line, last, start);
      ++i;
      last = start+1;
    } else if( start < end-1 && start[0] == '\r' && start[1] == '\n' ) {
      if( i >= start_line )
        R_rawToLines_makeStr(ret_data, i-start_line, last, start);
      ++i;
      ++start;
      last = start+1;
    }
    ++start;
    if( i == start_line + line_count )
      break;
  }
  if( i >= start_line && i < start_line + line_count )
    R_rawToLines_makeStr(ret_data, i-start_line, last, start);

  R_ReleaseObject(ret_data);
  return ret_data;
}

void R_init_uncompress(DllInfo* info) {
  R_CallMethodDef callMethods[] = {
    { "uncompress", (DL_FUNC)R_uncompress, 1 },
    { "rawToLines", (DL_FUNC)R_rawToLines, 3 },
    { NULL, NULL, 0 }
  };

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
