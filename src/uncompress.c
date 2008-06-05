
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

R_INLINE unsigned long read_bits(const unsigned char** start, const unsigned char* end, unsigned char* pos, int num_bits) {
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

R_INLINE unsigned char** init_table(unsigned char block_mode, unsigned long* table_size, unsigned long* table_alloc) {
  int i;
  unsigned char** ret;

  *table_alloc = 1024;
  *table_size = 256 + (block_mode ? 1 : 0);
  ret = malloc((*table_alloc)*sizeof(unsigned char*));
  for( i = 0; i < 256; ++i ) {
    ret[i] = malloc(4);
    ret[i][0] = 1;
    ret[i][1] = 0;
    ret[i][2] = (unsigned char)i;
  }
  memset(ret+256, 0, 768*sizeof(unsigned char*));

  return ret;
}
R_INLINE void reinit_table(unsigned char*** table, unsigned char block_mode, unsigned long* table_size, unsigned long* table_alloc) {
  *table_size = 256 + (block_mode ? 1 : 0);
}
R_INLINE void free_table(unsigned char** table, unsigned long table_alloc) {
  int i;

  for( i = 0; i < table_alloc; ++i ) {
    free(table[i]);
  }
  free(table);
}
R_INLINE void add_table_entry(unsigned char*** table, unsigned long* table_size, unsigned long* table_alloc, unsigned char* str, unsigned short str_len, unsigned char chr) {
  if( *table_size == *table_alloc ) {
    *table_alloc += 1024;
    *table = (unsigned char**)realloc(*table, (*table_alloc)*sizeof(unsigned char*));
    memset((*table) + *table_alloc - 1024, 0, 1024*sizeof(unsigned char*));
  }
  (*table)[*table_size] = malloc(str_len+3);
  (*table)[*table_size][0] = (str_len+1)&255;
  (*table)[*table_size][1] = (str_len+1)>>8;
  memcpy((*table)[*table_size]+2, str, str_len);
  (*table)[*table_size][2+str_len] = chr;
  *table_size += 1;
}

R_INLINE void append_ret(unsigned char** ret, unsigned long* ret_size, unsigned long* ret_alloc, unsigned char* str, unsigned short len) {
  if( *ret_size + len > *ret_alloc ) {
    *ret_alloc += 4096;
    *ret = realloc(*ret, *ret_alloc);
  }
  memcpy((*ret)+*ret_size, str, len);
  *ret_size += len;
}

SEXP R_uncompress(const SEXP data) {
  const unsigned char* start, * end;
  SEXP ret_data;

  unsigned char pos = 0;
  unsigned char info, block_mode, max_bits, table_bits, curstr_alloc, laststr_alloc;
  unsigned char** table;
  unsigned char* ret, * laststr, * curstr;
  unsigned long table_size, table_alloc, ret_size, ret_alloc, max_entries, curstr_len, laststr_len;

  if( TYPEOF(data) != RAWSXP )
    return R_NilValue;
  start = RAW_POINTER(data);
  end = start + GET_LENGTH(data);

  if( read_bits(&start, end, &pos, 8) != 31 || read_bits(&start, end, &pos, 8) != 157 ) {
    /* not valid data to uncompress */
    return R_NilValue;
  }
  info = read_bits(&start, end, &pos, 8);
  block_mode = info>>7;
  max_bits = info&31;
  table = init_table(block_mode, &table_size, &table_alloc);
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
  while(start < end) {
    unsigned long code = read_bits(&start, end, &pos, table_bits);
    if( start > end || (start == end && pos != 0 ) )
      break; /* end of data */
    if( block_mode && code == 256 ) {
      reinit_table(&table, block_mode, &table_size, &table_alloc);
      table_bits = 9;
      if( laststr_alloc )
        free(laststr);
      laststr = 0;
      laststr_alloc = 0;
      laststr_len = 0;
      start = (const char*)((((((unsigned long)(start))+12)>>4)<<4)+3);
    } else {
      if( code > table_size ) {
        /* invalid compressed data */
        free_table(table, table_alloc);
        free(ret);
        if( curstr_alloc )
          free(curstr);
        if( laststr_alloc )
          free(laststr);
        return R_NilValue;
      }
      if( code == table_size ) {
        unsigned char* temp = curstr;

        if( !laststr ) {
          /* invalid compressed data */
          free_table(table, table_alloc);
          free(ret);
          if( curstr_alloc )
            free(curstr);
          if( laststr_alloc )
            free(laststr);
          return R_NilValue;
        }

        if( curstr_alloc ) {
          curstr = realloc(curstr, curstr_len + 1);
          if( laststr == temp )
            laststr = curstr;
        } else {
          curstr = malloc(curstr_len + 1);
          memcpy(curstr, temp, curstr_len);
          curstr_alloc = 1;
        }
        curstr[curstr_len++] = laststr[0];
      } else {
        curstr_len = table[code][0] | (((unsigned short)table[code][1])<<8);
        if( curstr_alloc ) {
          if( laststr == curstr ) {
            laststr = malloc(laststr_len);
            memcpy(laststr, curstr, laststr_len);
            laststr_alloc = 1;
          }
          free(curstr);
        }
        curstr = table[code]+2;
        curstr_alloc = 0;
      }

      append_ret(&ret, &ret_size, &ret_alloc, curstr, curstr_len);

      if( table_size < max_entries && laststr ) {
        add_table_entry(&table, &table_size, &table_alloc, laststr, laststr_len, curstr[0]);
        if( table_size == (1<<table_bits) && table_bits != max_bits )
          ++table_bits;
      }

      if( laststr_alloc ) {
        free(laststr);
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
  free_table(table, table_alloc);

  ret_data = NEW_RAW(ret_size);
  memcpy(RAW_POINTER(ret_data), ret, ret_size);
  return ret_data;
}

void R_init_uncompress(DllInfo* info) {
  R_CallMethodDef callMethods[] = {
    { "uncompress", (DL_FUNC)R_uncompress, 1 },
    { NULL, NULL, 0 }
  };

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
