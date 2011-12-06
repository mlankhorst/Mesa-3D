/*
 * Copyright 2011 Maarten Lankhorst
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
 */

union pipe_desc {
   struct pipe_picture_desc *base;
   struct pipe_mpeg12_picture_desc *mpeg12;
   struct pipe_mpeg4_picture_desc *mpeg4;
   struct pipe_vc1_picture_desc *vc1;
   struct pipe_h264_picture_desc *h264;
};

typedef uint16_t b16;
typedef uint8_t b8;
typedef uint32_t b32;

struct strparm_bsp {
	uint32_t w0[4]; // bits 0-23 length, bits 24-31 addr_hi
	uint32_t w1[4]; // bit 8-24 addr_lo
	uint32_t unk20; // should be idx * 0x8000000, bitstream offset
	uint32_t do_crypto_crap; // set to 0
};

struct comm {
	uint32_t bsp_cur_index; // 000
	uint32_t byte_ofs; // 004
	uint32_t status[0x10]; // 008
	uint32_t pos[0x10]; // 048
	uint8_t pad[0x100 - 0x88]; // 0a0 bool comm_encrypted

	uint32_t pvp_cur_index; // 100
	uint32_t acked_byte_ofs; // 104
	uint32_t status_vp[0x10]; // 108
	uint16_t mb_y[0x10]; //148
	uint32_t pvp_stage; // 168 0xeeXX
	uint16_t parse_endpos_index; // 16c
	uint16_t irq_index; // 16e
	uint8_t  irq_470[0x10]; // 170
	uint32_t irq_pos[0x10]; // 180
	uint32_t parse_endpos[0x10]; // 1c0
};

struct mpeg12_picparm_bsp {
	uint16_t width;
	uint16_t height;
	uint8_t picture_structure;
	uint8_t picture_coding_type;
	uint8_t intra_dc_precision;
	uint8_t frame_pred_frame_dct;
	uint8_t concealment_motion_vectors;
	uint8_t intra_vlc_format;
	uint16_t pad;
	uint8_t f_code[2][2];
};

struct mpeg4_picparm_bsp {
	uint16_t width;
	uint16_t height;
	uint8_t vop_time_increment_size;
	uint8_t interlaced;
	uint8_t resync_marker_disable;
};

struct vc1_picparm_bsp {
	uint16_t width;
	uint16_t height;
	uint8_t profile; // 04 0 simple, 1 main, 2 advanced
	uint8_t postprocflag; // 05
	uint8_t pulldown; // 06
	uint8_t interlaced; // 07
	uint8_t tfcntrflag; // 08
	uint8_t finterpflag; // 09
	uint8_t psf; // 0a
	uint8_t pad; // 0b
	uint8_t multires; // 0c
	uint8_t syncmarker; // 0d
	uint8_t rangered; // 0e
	uint8_t maxbframes; // 0f
	uint8_t dquant; // 10
	uint8_t panscan_flag; // 11
	uint8_t refdist_flag; // 12
	uint8_t quantizer; // 13
	uint8_t extended_mv; // 14
	uint8_t extended_dmv; // 15
	uint8_t overlap; // 16
	uint8_t vstransform; // 17
};

struct h264_picparm_bsp {
	// 00
	b32 unk00;
	// 04
	b32 log2_max_frame_num_minus4; // 04 checked
	b32 pic_order_cnt_type; // 08 checked
	b32 log2_max_pic_order_cnt_lsb_minus4; // 0c checked
	b32 pad3; // 10, or unknown

	b32 frame_mbs_only_flag; // 14, always 1?
	b32 direct_8x8_inference_flag; // 18, always 1?
	b32 width_mb; // 1c checked
	b32 height_mb; // 20 checked
	// 24
	//struct picparm2
		b32 entropy_coding_mode_flag; // 00, checked
		b32 pic_order_present_flag; // 04 checked
		b32 unk; // 08 seems to be 0?
		b32 pad1; // 0c seems to be 0?
		b32 delta_pic_order_always_zero_flag; // 10 always 0 ?
		b32 num_ref_idx_l0_active_minus1; // 14 always 0?
		b32 num_ref_idx_l1_active_minus1; // 18 always 0?
		b32 weighted_pred_flag; // 1c checked
		b32 weighted_bipred_idc; // 20 checked
		b32 pic_init_qp_minus26; // 24 checked
		b32 deblocking_filter_control_present_flag; // 28 always 1?
		b32 redundant_pic_cnt_present_flag; // 2c always 0?
		b32 transform_8x8_mode_flag; // 30 checked
		b32 mb_adaptive_frame_field_flag; // 34 checked-ish
		b8 field_pic_flag; // 38 checked
		b8 bottom_field_flag; // 39 checked
		b8 real_pad[0x1b]; // XX why?
};

struct mpeg12_picparm_vp {
	b16 width; // 00 in mb units
	b16 height; // 02 in mb units

	b32 unk04; // 04 stride for Y?
	b32 unk08; // 08 stride for CbCr?

	b32 ofs[6]; // 1c..20 ofs
	b32 bucket_size; // 24
	b32 inter_ring_data_size; // 28
	b16 unk2c; // 2c
	b16 alternate_scan; // 2e
	b16 unk30; // 30 not seen set yet
	b16 picture_structure; // 32
	b16 pad2[3];
	b16 unk3a; // 3a set on I frame?

	b32 f_code[4]; // 3c
	b32 picture_coding_type; // 4c
	b32 intra_dc_precision; // 50
	b32 q_scale_type; // 54
	b32 top_field_first; // 58
	b32 full_pel_forward_vector; // 5c
	b32 full_pel_backward_vector; // 60
	b8 intra_quantizer_matrix[0x40]; // 64
	b8 non_intra_quantizer_matrix[0x40]; // a4
};

struct mpeg4_picparm_vp {
	b32 width; // 00 in normal units
	b32 height; // 04 in normal units
	b32 unk08; // stride 1
	b32 unk0c; // stride 2
	b32 ofs[6]; // 10..24 ofs
	b32 bucket_size; // 28
	b32 pad1; // 2c, pad
	b32 pad2; // 30
	b32 inter_ring_data_size; // 34

	b32 trd[2]; // 38, 3c
	b32 trb[2]; // 40, 44
	b32 u48; // XXX codec selection? Should test with different values of VdpDecoderProfile
	b16 f_code_fw; // 4c
	b16 f_code_bw; // 4e
	b8 pad3; // 50 unused?

	// unknown fields: interlaced, quarter_sample, top_field_first
	b8 u51; // bool, written to 528
	b8 u52; // bool, written to 548
	b8 u53; // bool, negated written to 528 shifted by 1
	b8 u54; // bool, written to 0x740
	b8 vop_coding_type; // 55
	b8 rounding_control; // 56
	b8 alternate_vertical_scan_flag; // 57 bool
	b8 u58; // bool, written to vuc

	b8 pad4[3]; // 59, 5a, 5b, contains garbage on blob
	b32 pad5[0x10]; // 5c...9c non-inclusive, but WHY?

	b32 intra[0x10]; // 9c
	b32 non_intra[0x10]; // bc
	// udc..uff pad?
};

// Full version, with data pumped from BSP
struct vc1_picparm_vp {
	b32 bucket_size; // 00
	b32 pad; // 04

	b32 inter_ring_data_size; // 08
	b32 unk0c; // stride 1
	b32 unk10; // stride 2
	b32 ofs[6]; // 14..28 ofs

	b16 width; // 2c
	b16 height; // 2e

	b8 profile; // 30 0 = simple, 1 = main, 2 = advanced
	b8 loopfilter; // 31 written into vuc
	b8 u32; // 32, written into vuc
	b8 dquant; // 33

	b8 overlap; // 34
	b8 quantizer; // 35
	b8 u36; // 36, bool
	b8 pad2; // 37, to align to 0x38
};

struct h264_picparm_vp { // 700..a00
	b16 width, height;
	b32 stride1, stride2; // 04 08
	b32 ofs[6]; // 0c..24 in-image offset

	b32 u24; // nfi ac8 ?
	b32 bucket_size; // 28 bucket size
	b32 inter_ring_data_size; // 2c

	unsigned f0 : 1; // 0 0x01: into 640 shifted by 3, 540 shifted by 5, half size something?
	unsigned f1 : 1; // 1 0x02: into vuc ofs 56
	unsigned weighted_pred_flag : 1; // 2 0x04
	unsigned f3 : 1; // 3 0x08: into vuc ofs 68
	unsigned is_reference : 1; // 4
	unsigned interlace : 1; // 5 !field_pic_flag
	unsigned bottom_field_flag : 1; // 6
	unsigned f7 : 1; // 7 0x80: nfi yet

	signed log2_max_frame_num_minus4 : 4; // 31 0..3
	unsigned u31_45 : 2; // 31 4..5
	unsigned pic_order_cnt_type : 2; // 31 6..7
	signed pic_init_qp_minus26 : 6; // 32 0..5
	signed chroma_qp_index_offset : 5; // 32 6..10
	signed second_chroma_qp_index_offset : 5; // 32 11..15

	unsigned weighted_bipred_idc : 2; // 34 0..1
	unsigned fifo_dec_index : 7; // 34 2..8
	unsigned tmp_idx : 5; // 34 9..13
	unsigned frame_number : 16; // 34 14..29
	unsigned u34_3030 : 1; // 34 30..30 pp.u34[30:30]
	unsigned u34_3131 : 1; // 34 31..31 pad?

	b32 field_order_cnt[2]; // 38, 3c

	struct { // 40
		// 0x00223102
		// nfi (needs: top_is_reference, bottom_is_reference, is_long_term, maybe some other state that was saved..
		unsigned fifo_idx : 7; // 00 0..6
		unsigned tmp_idx : 5; // 00 7..11
		unsigned unk12 : 1; // 00 12 not seen yet, but set, maybe top_is_reference
		unsigned unk13 : 1; // 00 13 not seen yet, but set, maybe bottom_is_reference?
		unsigned unk14 : 1; // 00 14 skipped?
		unsigned notseenyet : 1; // 00 15 pad?
		unsigned unk16 : 1; // 00 16
		unsigned unk17 : 4; // 00 17..20
		unsigned unk21 : 4; // 00 21..24
		unsigned pad : 7; // 00 d25..31

		b32 field_order_cnt[2]; // 04,08
		b32 frame_idx; // 0c
	} refs[0x10];

	b8 m4x4[6][16]; // 140
	b8 m8x8[2][64]; // 1a0
	b32 u220; // 220 number of extra reorder_list to append?
	b8 u224[0x20]; // 224..244 reorder_list append ?
	b8 nfi244[0xb8]; // add some pad to make sure nulls are read
};
