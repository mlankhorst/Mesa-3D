
#include "nv50_ir_util.h"

namespace nv50_ir {

void DLList::clear()
{
   for (Item *next, *item = head.next; item != &head; item = next) {
      next = item->next;
      delete item;
   }
   head.next = head.prev = &head;
}

void
DLList::Iterator::erase()
{
   Item *rem = pos;

   if (rem == term)
      return;
   pos = pos->next;

   DLLIST_DEL(rem);
   delete rem;
}

void DLList::Iterator::moveToList(DLList& dest)
{
   Item *item = pos;

   assert(term != &dest.head);
   assert(pos != term);

   pos = pos->next;

   DLLIST_DEL(item);
   DLLIST_ADDHEAD(&dest.head, item);
}

bool
DLList::Iterator::insert(void *data)
{
   Item *ins = new Item(data);

   ins->next = pos->next;
   ins->prev = pos;
   pos->next->prev = ins;
   pos->next = ins;

   if (pos == term)
      term = ins;

   return true;
}

void
Stack::moveTo(Stack& that)
{
   unsigned int newSize = this->size + that.size;

   while (newSize > that.limit)
      that.resize();
   memcpy(&that.array[that.size], &array[0], this->size * sizeof(Item));

   that.size = newSize;
   this->size = 0;
}

ArrayList::ArrayList()
{
   array = NULL;
   size = 0;
   limit = 0;
}

ArrayList::~ArrayList()
{
   if (array)
      FREE(array);
}

void
ArrayList::reallocate(int capacity)
{
   unsigned int oldSize = sizeof(void *) * limit;
   unsigned int newSize = sizeof(void *) * capacity;

   array = (void **)REALLOC(array, oldSize, newSize);

   memset(&array[limit], 0, newSize - oldSize);

   limit = capacity;
}

Interval::~Interval()
{
   for (Range *next, *r = head; r; r = next) {
      next = r->next;
      delete r;
   }
}

bool
Interval::extend(int a, int b)
{
   Range *r, **nextp = &head;

   // NOTE: we need empty intervals for fixed registers
   // if (a == b)
   //   return false;
   assert(a <= b);

   for (r = head; r; r = r->next) {
      if (b < r->bgn)
         break; // insert before
      if (a > r->end) {
         // insert after
         nextp = &r->next;
         continue;
      }

      // overlap
      if (a < r->bgn) {
         r->bgn = a;
         if (b > r->end)
            r->end = b;
         r->coalesce(&tail);
         return true;
      }
      if (b > r->end) {
         r->end = b;
         r->coalesce(&tail);
         return true;
      }
      assert(a >= r->bgn);
      assert(b <= r->end);
      return true;
   }

   (*nextp) = new Range(a, b);
   (*nextp)->next = r;

   for (r = (*nextp); r->next; r = r->next);
   tail = r;
   return true;
}

bool Interval::contains(int pos)
{
   for (Range *r = head; r; r = r->next)
      if (pos >= r->bgn && r->end <= pos)
         return true;
   return false;
}

bool Interval::overlaps(const Interval &iv) const
{
   for (Range *rA = this->head; rA; rA = rA->next)
      for (Range *rB = iv.head; rB; rB = rB->next)
         if (rB->bgn < rA->end &&
             rB->end > rA->bgn)
            return true;
   return false;
}

void Interval::unify(Interval &that)
{
   assert(this != &that);
   for (Range *next, *r = that.head; r; r = next) {
      next = r->next;
      this->extend(r->bgn, r->end);
      delete r;
   }
   that.head = NULL;
}

void Interval::print() const
{
   if (!head)
      return;
   INFO("[%i %i)", head->bgn, head->end);
   for (const Range *r = head->next; r; r = r->next)
      INFO(" [%i %i)", r->bgn, r->end);
   INFO("\n");
}

void
BitSet::andNot(const BitSet &set)
{
   assert(data && set.data);
   assert(size >= set.size);
   for (unsigned int i = 0; i < (set.size + 31) / 32; ++i)
      data[i] &= ~set.data[i];
}

BitSet& BitSet::operator|=(const BitSet &set)
{
   assert(data && set.data);
   assert(size >= set.size);
   for (unsigned int i = 0; i < (set.size + 31) / 32; ++i)
      data[i] |= set.data[i];
   return *this;
}

bool BitSet::allocate(unsigned int nBits, bool zero)
{
   if (data && size < nBits) {
      FREE(data);
      data = NULL;
   }
   size = nBits;

   if (!data)
      data = reinterpret_cast<uint32_t *>(CALLOC((size + 31) / 32, 4));

   if (zero)
      memset(data, 0, (size + 7) / 8);
   else
      data[(size + 31) / 32 - 1] = 0; // clear unused bits (e.g. for popCount)

   return data;
}

unsigned int BitSet::popCount() const
{
   unsigned int count = 0;

   for (unsigned int i = 0; i < (size + 31) / 32; ++i)
      if (data[i])
         count += util_bitcount(data[i]);
   return count;
}

void BitSet::fill(uint32_t val)
{
   unsigned int i;
   for (i = 0; i < (size + 31) / 32; ++i)
      data[i] = val;
   if (val)
      data[i] &= ~(0xffffffff << (size % 32)); // BE ?
}

void BitSet::setOr(BitSet *pA, BitSet *pB)
{
   if (!pB) {
      *this = *pA;
   } else {
      for (unsigned int i = 0; i < (size + 31) / 32; ++i)
         data[i] = pA->data[i] | pB->data[i];
   }
}

void BitSet::print() const
{
   unsigned int n = 0;
   INFO("BitSet of size %u:\n", size);
   for (unsigned int i = 0; i < (size + 31) / 32; ++i) {
      uint32_t bits = data[i];
      while (bits) {
         int pos = ffs(bits) - 1;
         bits &= ~(1 << pos);
         INFO(" %i", i * 32 + pos);
         ++n;
         if ((n % 16) == 0)
            INFO("\n");
      }
   }
   if (n % 16)
      INFO("\n");
}

} // namespace nv50_ir