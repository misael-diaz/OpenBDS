#ifndef GUARD_OPENBDS_BDS_TYPES_H
#define GUARD_OPENBDS_BDS_TYPES_H

#include<stdint.h>

union __OBDS_PROP_TYPE__
{
  double data;
  uint64_t bin;
};

typedef union __OBDS_PROP_TYPE__ prop_t;

#endif
