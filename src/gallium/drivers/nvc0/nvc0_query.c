/*
 * Copyright 2011 Nouveau Project
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Authors: Christoph Bumiller
 */

#include "util/u_debug.h"

#include "nvc0_context.h"
#include "nouveau/nv_object.xml.h"

/* XXX: Nested queries, and simultaneous queries on multiple gallium contexts
 * (since we use only a single GPU channel per screen) will not work properly.
 *
 * The first is not that big of an issue because OpenGL does not allow nested
 * queries anyway.
 */

struct nvc0_query {
   uint32_t *data;
   uint32_t type;
   uint32_t sequence;
   struct nouveau_bo *bo;
   uint32_t base;
   uint32_t offset; /* base + i * 16 */
   boolean ready;
   boolean is64bit;
   struct nouveau_mm_allocation *mm;
};

static const char *pipeline_statistics_names[] =
{
   "VFETCH/VERTICES",
   "VFETCH/PRIMS",
   "VP/LAUNCHES",
   "GP/LAUNCHES",
   "GP/PRIMS_OUT",
   "RAST/PRIMS_IN",
   "RAST/PRIMS_DRAWN",
   "ROP/PIXELS",
   "TCP/LAUNCHES",
   "TEP/LAUNCHES"
};

#define NVC0_QUERY_ALLOC_SPACE 128

static INLINE struct nvc0_query *
nvc0_query(struct pipe_query *pipe)
{
   return (struct nvc0_query *)pipe;
}

static boolean
nvc0_query_allocate(struct nvc0_context *nvc0, struct nvc0_query *q, int size)
{
   struct nvc0_screen *screen = nvc0->screen;
   int ret;

   if (q->bo) {
      nouveau_bo_ref(NULL, &q->bo);
      if (q->mm) {
         if (q->ready)
            nouveau_mm_free(q->mm);
         else
            nouveau_fence_work(screen->base.fence.current, nouveau_mm_free_work, q->mm);
      }
   }
   if (size) {
      q->mm = nouveau_mm_allocate(screen->base.mm_GART, size, &q->bo, &q->base);
      if (!q->bo)
         return FALSE;
      q->offset = q->base;

      ret = nouveau_bo_map_range(q->bo, q->base, size, NOUVEAU_BO_RD |
                                 NOUVEAU_BO_NOSYNC);
      if (ret) {
         nvc0_query_allocate(nvc0, q, 0);
         return FALSE;
      }
      q->data = q->bo->map;
      nouveau_bo_unmap(q->bo);
   }
   return TRUE;
}

static void
nvc0_query_destroy(struct pipe_context *pipe, struct pipe_query *pq)
{
   nvc0_query_allocate(nvc0_context(pipe), nvc0_query(pq), 0);
   FREE(nvc0_query(pq));
}

static struct pipe_query *
nvc0_query_create(struct pipe_context *pipe, unsigned type)
{
   struct nvc0_context *nvc0 = nvc0_context(pipe);
   struct nvc0_query *q;
   unsigned space = NVC0_QUERY_ALLOC_SPACE;

   q = CALLOC_STRUCT(nvc0_query);
   if (!q)
      return NULL;

   if (type == PIPE_QUERY_PIPELINE_STATISTICS)
      space = NVC0_QUERY_ALLOC_SPACE * 4;

   if (!nvc0_query_allocate(nvc0, q, space)) {
      FREE(q);
      return NULL;
   }

   q->is64bit = (type == PIPE_QUERY_PRIMITIVES_GENERATED ||
                 type == PIPE_QUERY_PRIMITIVES_EMITTED ||
                 type == PIPE_QUERY_SO_STATISTICS ||
                 type == PIPE_QUERY_PIPELINE_STATISTICS);
   q->type = type;

   if (q->type == PIPE_QUERY_OCCLUSION_COUNTER) {
      q->offset -= 16;
      q->data -= 16 / sizeof(*q->data); /* we advance before query_begin ! */
   }

   return (struct pipe_query *)q;
}

static void
nvc0_query_get(struct nouveau_channel *chan, struct nvc0_query *q,
               unsigned offset, uint32_t get)
{
   offset += q->offset;

   MARK_RING (chan, 5, 2);
   BEGIN_RING(chan, RING_3D(QUERY_ADDRESS_HIGH), 4);
   OUT_RELOCh(chan, q->bo, offset, NOUVEAU_BO_GART | NOUVEAU_BO_WR);
   OUT_RELOCl(chan, q->bo, offset, NOUVEAU_BO_GART | NOUVEAU_BO_WR);
   OUT_RING  (chan, q->sequence);
   OUT_RING  (chan, get);
}

static void
nvc0_query_begin(struct pipe_context *pipe, struct pipe_query *pq)
{
   struct nvc0_context *nvc0 = nvc0_context(pipe);
   struct nouveau_channel *chan = nvc0->screen->base.channel;
   struct nvc0_query *q = nvc0_query(pq);

   /* For occlusion queries we have to change the storage, because a previous
    * query might set the initial render conition to FALSE even *after* we re-
    * initialized it to TRUE.
    */
   if (q->type == PIPE_QUERY_OCCLUSION_COUNTER) {
      q->offset += 16;
      q->data += 16 / sizeof(*q->data);
      if (q->offset - q->base == NVC0_QUERY_ALLOC_SPACE)
         nvc0_query_allocate(nvc0, q, NVC0_QUERY_ALLOC_SPACE);

      /* XXX: can we do this with the GPU, and sync with respect to a previous
       *  query ?
       */
      q->data[1] = 1; /* initial render condition = TRUE */
   }
   if (!q->is64bit)
      q->data[0] = q->sequence++; /* the previously used one */

   switch (q->type) {
   case PIPE_QUERY_OCCLUSION_COUNTER:
      IMMED_RING(chan, RING_3D(COUNTER_RESET), NVC0_3D_COUNTER_RESET_SAMPLECNT);
      IMMED_RING(chan, RING_3D(SAMPLECNT_ENABLE), 1);
      break;
   case PIPE_QUERY_PRIMITIVES_GENERATED: /* store before & after instead ? */
      IMMED_RING(chan, RING_3D(COUNTER_RESET),
                 NVC0_3D_COUNTER_RESET_GENERATED_PRIMITIVES);
      break;
   case PIPE_QUERY_PRIMITIVES_EMITTED:
      IMMED_RING(chan, RING_3D(COUNTER_RESET),
                 NVC0_3D_COUNTER_RESET_EMITTED_PRIMITIVES);
      break;
   case PIPE_QUERY_SO_STATISTICS:
      BEGIN_RING_NI(chan, RING_3D(COUNTER_RESET), 2);
      OUT_RING  (chan, NVC0_3D_COUNTER_RESET_EMITTED_PRIMITIVES);
      OUT_RING  (chan, NVC0_3D_COUNTER_RESET_GENERATED_PRIMITIVES);
      break;
   case PIPE_QUERY_TIMESTAMP_DISJOINT:
   case PIPE_QUERY_TIME_ELAPSED:
      nvc0_query_get(chan, q, 0x10, 0x00005002);
      break;
   case PIPE_QUERY_PIPELINE_STATISTICS:
      nvc0_query_get(chan, q, 0xc0 + 0x00, 0x00801002); /* VFETCH, VERTICES */
      nvc0_query_get(chan, q, 0xc0 + 0x10, 0x01801002); /* VFETCH, PRIMS */
      nvc0_query_get(chan, q, 0xc0 + 0x20, 0x02802002); /* VP, LAUNCHES */
      nvc0_query_get(chan, q, 0xc0 + 0x30, 0x03806002); /* GP, ? */
      nvc0_query_get(chan, q, 0xc0 + 0x40, 0x04806002); /* GP, ? */
      // nvc0_query_get(chan, q, 0xc0 + 0x50, 0x05805002); /* SO, EMIT */
      // nvc0_query_get(chan, q, 0xc0 + 0x60, 0x06805002); /* SO, ? */
      nvc0_query_get(chan, q, 0xc0 + 0x50, 0x07804002); /* RAST, PRIMS_IN */
      nvc0_query_get(chan, q, 0xc0 + 0x60, 0x08804002); /* RAST, PRIMS_OUT */
      nvc0_query_get(chan, q, 0xc0 + 0x70, 0x0980a002); /* ROP, PIXELS */
      // nvc0_query_get(chan, q, 0xc0 + 0xa0, 0x0a80f002); /* CROP, ? */
      // nvc0_query_get(chan, q, 0xc0 + 0xb0, 0x0c80f002); /* CROP, ? */
      nvc0_query_get(chan, q, 0xc0 + 0x80, 0x0d808002); /* TCP, LAUNCHES ? */
      nvc0_query_get(chan, q, 0xc0 + 0x90, 0x0e809002); /* TEP, LAUNCHES ? */
      nvc0_query_get(chan, q, 0xc0 + 0xa0, 0x0f809002); /* TEP, ? */
      break;
   default:
      break;
   }
   q->ready = FALSE;
}

static void
nvc0_query_end(struct pipe_context *pipe, struct pipe_query *pq)
{
   struct nvc0_context *nvc0 = nvc0_context(pipe);
   struct nouveau_channel *chan = nvc0->screen->base.channel;
   struct nvc0_query *q = nvc0_query(pq);

   const int index = 0; /* for multiple vertex streams */

   switch (q->type) {
   case PIPE_QUERY_OCCLUSION_COUNTER:
      nvc0_query_get(chan, q, 0, 0x0100f002);
      BEGIN_RING(chan, RING_3D(SAMPLECNT_ENABLE), 1);
      OUT_RING  (chan, 0);
      break;
   case PIPE_QUERY_PRIMITIVES_GENERATED:
      nvc0_query_get(chan, q, 0, 0x09005002 | (index << 5));
      break;
   case PIPE_QUERY_PRIMITIVES_EMITTED:
      nvc0_query_get(chan, q, 0, 0x05805002 | (index << 5));
      break;
   case PIPE_QUERY_SO_STATISTICS:
      nvc0_query_get(chan, q, 0x00, 0x05805002 | (index << 5));
      nvc0_query_get(chan, q, 0x10, 0x09005002 | (index << 5));
      break;
   case PIPE_QUERY_TIMESTAMP_DISJOINT:
   case PIPE_QUERY_TIME_ELAPSED:
      nvc0_query_get(chan, q, 0, 0x00005002);
      break;
   case PIPE_QUERY_GPU_FINISHED:
      nvc0_query_get(chan, q, 0, 0x1000f010);
      break;
   case PIPE_QUERY_PIPELINE_STATISTICS:
      nvc0_query_get(chan, q, 0x00, 0x00801002); /* VFETCH, VERTICES */
      nvc0_query_get(chan, q, 0x10, 0x01801002); /* VFETCH, PRIMS */
      nvc0_query_get(chan, q, 0x20, 0x02802002); /* VP, LAUNCHES */
      nvc0_query_get(chan, q, 0x30, 0x03806002); /* GP, ? */
      nvc0_query_get(chan, q, 0x40, 0x04806002); /* GP, ? */
      // nvc0_query_get(chan, q, 0x50, 0x05805002); /* SO, EMIT */
      // nvc0_query_get(chan, q, 0x60, 0x06805002); /* SO, ? */
      nvc0_query_get(chan, q, 0x50, 0x07804002); /* RAST, PRIMS_IN */
      nvc0_query_get(chan, q, 0x60, 0x08804002); /* RAST, PRIMS_OUT */
      nvc0_query_get(chan, q, 0x70, 0x0980a002); /* ROP, PIXELS */
      // nvc0_query_get(chan, q, 0xa0, 0x0a80f002); /* CROP, ? */
      // nvc0_query_get(chan, q, 0xb0, 0x0c80f002); /* CROP, ? */
      nvc0_query_get(chan, q, 0x80, 0x0d808002); /* TCP, LAUNCHES ? */
      nvc0_query_get(chan, q, 0x90, 0x0e809002); /* TEP, LAUNCHES ? */
      nvc0_query_get(chan, q, 0xa0, 0x0f809002); /* TEP, ? */
      break;
   default:
      assert(0);
      break;
   }
}

static INLINE boolean
nvc0_query_ready(struct nvc0_query *q)
{
   return q->ready || (!q->is64bit && (q->data[0] == q->sequence));
}

static INLINE boolean
nvc0_query_wait(struct nvc0_query *q)
{
   int ret = nouveau_bo_map(q->bo, NOUVEAU_BO_RD);
   if (ret)
      return FALSE;
   nouveau_bo_unmap(q->bo);
   return TRUE;
}

static boolean
nvc0_query_result(struct pipe_context *pipe, struct pipe_query *pq,
                  boolean wait, void *result)
{
   struct nvc0_query *q = nvc0_query(pq);
   uint64_t *res64 = result;
   uint32_t *res32 = result;
   boolean *res8 = result;
   uint64_t *data64 = (uint64_t *)q->data;
   unsigned i;

   if (q->type == PIPE_QUERY_GPU_FINISHED) {
      res8[0] = nvc0_query_ready(q);
      return TRUE;
   }

   if (!q->ready) /* update ? */
      q->ready = nvc0_query_ready(q);
   if (!q->ready) {
      struct nouveau_channel *chan = nvc0_context(pipe)->screen->base.channel;
      if (!wait) {
         if (nouveau_bo_pending(q->bo) & NOUVEAU_BO_WR) /* for daft apps */
            FIRE_RING(chan);
         return FALSE;
      }
      debug_printf("waiting for query ...");
      if (!nvc0_query_wait(q))
         return FALSE;
      debug_printf(" ready\n");
   }
   q->ready = TRUE;

   switch (q->type) {
   case PIPE_QUERY_OCCLUSION_COUNTER: /* u32 sequence, u32 count, u64 time */
      res32[0] = q->data[1];
      break;
   case PIPE_QUERY_PRIMITIVES_GENERATED: /* u64 count, u64 time */
   case PIPE_QUERY_PRIMITIVES_EMITTED: /* u64 count, u64 time */
      res64[0] = data64[0];
      break;
   case PIPE_QUERY_SO_STATISTICS:
      res64[0] = data64[0];
      res64[1] = data64[1];
      break;
   case PIPE_QUERY_TIMESTAMP_DISJOINT: /* u32 sequence, u32 0, u64 time */
      res64[0] = 1000000000;
      res8[8] = (data64[0] == data64[2]) ? FALSE : TRUE;
      break;
   case PIPE_QUERY_TIME_ELAPSED:
      res64[0] = data64[1] - data64[3];
      break;
   case PIPE_QUERY_PIPELINE_STATISTICS:
      for (i = 0; i < 10; ++i) {
         res64[i] = data64[i * 2] - data64[24 + i * 2];
	 debug_printf("%s: %"PRIu64"\n",
		      pipeline_statistics_names[i], res64[i]);
      }
      break;
   default:
      return FALSE;
   }

   return TRUE;
}

static void
nvc0_render_condition(struct pipe_context *pipe,
                      struct pipe_query *pq, uint mode)
{
   struct nvc0_context *nvc0 = nvc0_context(pipe);
   struct nouveau_channel *chan = nvc0->screen->base.channel;
   struct nvc0_query *q;

   if (debug_get_bool_option("NV50_NO_RENDERING", FALSE)) {
      IMMED_RING(chan, RING_3D(COND_MODE), NVC0_3D_COND_MODE_NEVER);
      return;
   }

   if (!pq) {
      IMMED_RING(chan, RING_3D(COND_MODE), NVC0_3D_COND_MODE_ALWAYS);
      return;
   }
   q = nvc0_query(pq);

   if (mode == PIPE_RENDER_COND_WAIT ||
       mode == PIPE_RENDER_COND_BY_REGION_WAIT) {
      MARK_RING (chan, 5, 2);
      BEGIN_RING(chan, RING_3D_(NV84_SUBCHAN_QUERY_ADDRESS_HIGH), 4);
      OUT_RELOCh(chan, q->bo, q->offset, NOUVEAU_BO_GART | NOUVEAU_BO_RD);
      OUT_RELOCl(chan, q->bo, q->offset, NOUVEAU_BO_GART | NOUVEAU_BO_RD);
      OUT_RING  (chan, q->sequence);
      OUT_RING  (chan, 0x00001001);
   }

   MARK_RING (chan, 4, 2);
   BEGIN_RING(chan, RING_3D(COND_ADDRESS_HIGH), 3);
   OUT_RELOCh(chan, q->bo, q->offset, NOUVEAU_BO_GART | NOUVEAU_BO_RD);
   OUT_RELOCl(chan, q->bo, q->offset, NOUVEAU_BO_GART | NOUVEAU_BO_RD);
   OUT_RING  (chan, NVC0_3D_COND_MODE_RES_NON_ZERO);
}

void
nvc0_init_query_functions(struct nvc0_context *nvc0)
{
   struct pipe_context *pipe = &nvc0->base.pipe;

   pipe->create_query = nvc0_query_create;
   pipe->destroy_query = nvc0_query_destroy;
   pipe->begin_query = nvc0_query_begin;
   pipe->end_query = nvc0_query_end;
   pipe->get_query_result = nvc0_query_result;
   pipe->render_condition = nvc0_render_condition;
}
