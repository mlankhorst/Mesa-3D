
#ifndef __NV50_IR_DRIVER_H__
#define __NV50_IR_DRIVER_H__

#include "pipe/p_shader_tokens.h"

#include "tgsi/tgsi_util.h"
#include "tgsi/tgsi_parse.h"
#include "tgsi/tgsi_scan.h"

/*
 * This struct constitutes linkage information in TGSI terminology.
 *
 * It is created by the code generator and handed to the pipe driver
 * for input/output slot assignment.
 */
struct nv50_ir_varying
{
   uint8_t slot[4]; /* native slots for xyzw (address / 4) */

   unsigned mask     : 4; /* vec4 mask */
   unsigned linear   : 1; /* linearly interpolated if true */
   unsigned flat     : 1;
   unsigned centroid : 1;
   unsigned patch    : 1; /* patch constant value */
   unsigned regular  : 1; /* normal input/output or special if false */
   unsigned input    : 1; /* indicates direction of system values */
   unsigned oread    : 1; /* true if output is read from parallel TCP */

   ubyte id; /* TGSI register index */
   ubyte sn; /* TGSI semantic name */
   ubyte si; /* TGSI semantic index */
};

#define NV50_PROGRAM_IR_TGSI 0
#define NV50_PROGRAM_IR_SM4  1
#define NV50_PROGRAM_IR_GLSL 2
#define NV50_PROGRAM_IR_LLVM 3

struct nv50_ir_prog_info
{
   uint16_t type;       /* PIPE_SHADER */
   uint16_t targetArch; /* chipset (0x50,0x84,...) */

   struct {
      int16_t maxGPR;    /* may be -1 if none used */
      int16_t maxOutput;
      uint32_t *code;
      uint32_t codeSize;
      uint32_t tlsSize;
      uint8_t sourceFmt; /* NV50_PROGRAM_IR */
      const void *source;
      void *relocData;
   } bin;

   struct nv50_ir_varying sv[PIPE_MAX_SHADER_INPUTS];
   struct nv50_ir_varying in[PIPE_MAX_SHADER_INPUTS];
   struct nv50_ir_varying out[PIPE_MAX_SHADER_OUTPUTS];
   uint8_t numInputs;
   uint8_t numOutputs;
   uint8_t numPatchConstants; /* also included in numInputs/numOutputs */
   uint8_t numSysVals;

   struct {
      uint32_t *buffer;    /* for IMMEDIATE_ARRAY */
      uint16_t bufferSize; /* size of immediate array */
      uint16_t count;      /* count of inline immediates */
      uint32_t *data;      /* inline immediate data */
      ubyte *type;         /* for each (128 bit) vec4 */
   } immd;

   union {
      struct {
         uint32_t inputMask[4]; /* mask of attributes read (1 bit per scalar) */
      } vp;
      struct {
         uint8_t inputPatchSize;
         uint8_t outputPatchSize;
         uint8_t partitioning;    /* PIPE_TESS_PART */
         int8_t winding;          /* +1 (clockwise) / -1 (counter-clockwise) */
         uint8_t domain;          /* PIPE_PRIM_{QUADS,TRIANGLES,LINES} */
         uint8_t outputPrim;      /* PIPE_PRIM_{TRIANGLES,LINES,POINTS} */
      } tp;
      struct {
         uint8_t inputPrim;
         uint8_t outputPrim;
         unsigned instanceCount;
         unsigned maxVertices;
      } gp;
      struct {
         unsigned numColourResults;
         boolean writesDepth;
         boolean earlyFragTests;
         boolean separateFragData;
         boolean usesDiscard;
      } fp;
   } prop;

   struct {
      uint8_t clipDistance;
      uint8_t clipDistanceCount;
      uint8_t cullDistanceMask;  /* clip distance mode (1 bit per output) */
      uint8_t pointSize;         /* output index for PointSize */
      uint8_t edgeFlagIn;
      uint8_t edgeFlagOut;
      uint8_t fragDepth;         /* output index for FragDepth */
      uint8_t sampleMask;        /* output index for SampleMask */
      uint8_t backFaceColor[2];  /* input/output index for BFC */
   } io;

   boolean requireTLS;
   boolean globalStoresEnabled;

   int (*assignSlots)(struct nv50_ir_prog_info *);
};

#ifdef __cplusplus
extern "C" {
#endif

extern int nv50_ir_generate_code(struct nv50_ir_prog_info *);

extern void nv50_ir_relocate_code(void *relocData, uint32_t *code,
                                  uint32_t codePos,
                                  uint32_t libPos,
                                  uint32_t dataPos);

extern void nv50_ir_get_target_library(uint32_t chipset,
                                       const uint32_t **code, uint32_t *size);

#ifdef __cplusplus
}
#endif

#endif // __NV50_IR_DRIVER_H__