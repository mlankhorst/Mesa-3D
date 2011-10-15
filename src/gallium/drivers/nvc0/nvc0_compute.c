
int
nvc0_compute_screen_init(struct nvc0_screen *screen)
{
   struct nouveau_channel *chan = screen->base.channel;
   struct nouveau_grobj *djka;
   int ret;
   uint64_t value;

   ret = nouveau_grobj_alloc(chan, 0xbeef90c0, NVC0_COMPUTE, &screen->djka);
   if (ret)
      return ret;
   djka = screen->djka;

   nouveau_device_get_param(chan->device, NOUVEAU_GETPARAM_GRAPH_UNITS, &value);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_MP_LIMIT, 1);
   OUT_RING  (chan, CLAMP(value, 1, 16));
   BEGIN_RING(chan, djka, NVC0_COMPUTE_CALL_LIMIT_LOG, 1);
   OUT_RING  (chan, 0xf); /* unlimited */
   BEGIN_RING(chan, djka, NVC0_COMPUTE_LOCAL_POS_ALLOC, 3);
   OUT_RING  (chan, 0x80); /* sizeof l[> 0] (tls) */
   OUT_RING  (chan, 0x400); /* sizeof l[< 0] (thread's normal stack) */
   OUT_RING  (chan, 0x800); /* size of control flow stack (per warp) */
   BEGIN_RING(chan, djka, NVC0_COMPUTE_CACHE_SPLIT, 1);
   OUT_RING  (chan, NVC0_COMPUTE_CACHE_SPLIT_16K_SHARED_48K_L1);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_SHARED_BASE, 1);
   OUT_RING  (chan, 0);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_SHARED_SIZE, 1);
   OUT_RING  (chan, 0);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_TEX_LIMITS, 1);
   OUT_RING  (chan, 0x54);

   BEGIN_RING(chan, djka, 0x02a0, 1);
   OUT_RING  (chan, 0x8000);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_GRIDDIM_YX, 2);
   OUT_RING  (chan, (1 << 16) | 1);
   OUT_RING  (chan, 1);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_BLOCKDIM_YX, 2);
   OUT_RING  (chan, (1 << 16) | 1);
   OUT_RING  (chan, 1);

   BEGIN_RING(chan, djka, 0x02c4, 1);
   OUT_RING  (chan, 0);
   BEGIN_RING_NI(chan, djka, NVC0_COMPUTE_GLOBAL_BASE, 0x100);
   for (i = 0; i <= 0xff; ++i)
      OUT_RING(chan, (0xc << 28) | (i << 16) | i);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_FLUSH, 1);
   OUT_RING  (chan, NVC0_COMPUTE_FLUSH_CODE);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_CB_SIZE, 3);
   OUT_RING  (chan, 256);
   OUT_RELOCh(chan, parmbuf, 0, NOUVEAU_BO_VRAM | NOUVEAU_BO_WR);
   OUT_RELOCl(chan, parmbuf, 0, NOUVEAU_BO_VRAM | NOUVEAU_BO_WR);

   return 0;
}

void
nvc0_compute_launch_program(struct nvc0_program *prog)
{
   unsigned i;

   if (prog->shared_size > (16 << 10)) {
      BEGIN_RING(chan, djka, NVC0_COMPUTE_CACHE_SPLIT, 1);
      OUT_RING  (chan, NVC0_COMPUTE_CACHE_SPLIT_48K_SHARED_16K_L1);
   }
   BEGIN_RING(chan, djka, NVC0_COMPUTE_SHARED_SIZE, 1);
   OUT_RING  (chan, prog->shared_size);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_GRIDDIM_YX, 2);
   OUT_RING  (chan, (prog->cp.gdy << 16) | prog->cp.gdx);
   OUT_RING  (chan, prog->cp.gdz);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_BLOCKDIM_YX, 2);
   OUT_RING  (chan, (prog->cp.bdy << 16) | prog->cp.bdx);
   OUT_RING  (chan, prog->cp.bdz);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_CB_SIZE, 3);
   OUT_RING  (chan, 256);
   OUT_RELOCh(chan, parmbuf, 0, NOUVEAU_BO_VRAM | NOUVEAU_BO_WR);
   OUT_RELOCl(chan, parmbuf, 0, NOUVEAU_BO_VRAM | NOUVEAU_BO_WR);
   BEGIN_RING_1I(chan, djka, NVC0_COMPUTE_CB_POS, 1 + parm->size / 4);
   OUT_RING  (chan, 0);
   for (i = 0; i < parm->size / 4; ++i)
      OUT_RING(chan, parm->data[i]);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_CP_START_ID, 1);
   OUT_RING  (chan, prog->code_base);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_THREADS_ALLOC, 2);
   OUT_RING  (chan, prog->cp.bdx * prog->cp.bdy * prog->cp.bdz);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_CP_GPR_ALLOC, 1);
   OUT_RING  (chan, prog->max_gpr);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_GRIDID, 1);
   OUT_RING  (chan, 0);
   BEGIN_RING(chan, djka, 0x036c, 1);
   OUT_RING  (chan, 0);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_FLUSH, 1);
   OUT_RING  (chan, NVC0_COMPUTE_FLUSH_GLOBAL | NVC0_COMPUTE_FLUSH_UNK8);

   BEGIN_RING(chan, djka, NVC0_COMPUTE_BEGIN, 1);
   OUT_RING  (chan, 0);
   BEGIN_RING(chan, djka, 0x0a08, 1);
   OUT_RING  (chan, 0);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_LAUNCH, 1);
   OUT_RING  (chan, 0);
   BEGIN_RING(chan, djka, NVC0_COMPUTE_END, 1);
   OUT_RING  (chan, 0);
   BEGIN_RING(chan, djka, 0x0360, 1);
   OUT_RING  (chan, 1);
}
