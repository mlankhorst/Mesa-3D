
#ifndef __NV50_IR_UTIL_H__
#define __NV50_IR_UTIL_H__

#undef NDEBUG // XXX: argh, build system
#include <new>
#include <assert.h>
#include <stdio.h>

#include "util/u_inlines.h"
#include "util/u_memory.h"

#define ERROR(args...) fprintf(stderr, "ERROR: " args)
#define WARN(args...) fprintf(stderr, "WARNING: " args)
#define INFO(args...) fprintf(stderr, args)

#define OOM() \
   do {                                                           \
      fprintf(stderr, "memory allocation failed");                \
      abort();                                                    \
   } while(0)

#if 0

# define New_Instruction(fn, args...) \
   new ((fn)->getProgram()->InstructionPool.allocate()) Instruction(func, args)

# define Del_Instruction(prog, insn) (prog)->releaseInstruction(insn)

# define New_LValue(fn, args...) \
   new ((fn)->getProgram()->LValuePool.allocate()) LValue(fn, args)

# define Del_Value(prog, value) (prog)->releaseValue(value)

# define New_Symbol(prog, args...) new Symbol(prog, args)
# define New_ImmediateValue(prog, args...) new ImmediateValue(prog, args)

#else

# define New_Instruction(args...) new Instruction(args)
# define New_LValue(args...) new LValue(args)
# define New_Symbol(args...) new Symbol(args)
# define New_ImmediateValue(args...) new ImmediateValue(args)

# define Del_Instruction(prog, insn) delete insn
# define Del_Value(prog, value) delete value

#endif

namespace nv50_ir {

class Iterator
{
public:
   virtual void next() = 0;
   virtual void *get() const = 0;
   virtual bool end() const = 0; // if true, get will return 0
};

class ManipIterator : public Iterator
{
public:
   virtual bool insert(void *) = 0; // insert after current position
   virtual void erase() = 0;
};

// WARNING: do not use a->prev/next for __item or __list

#define DLLIST_DEL(__item)                      \
   do {                                         \
      (__item)->prev->next = (__item)->next;    \
      (__item)->next->prev = (__item)->prev;    \
      (__item)->next = (__item);                \
      (__item)->prev = (__item);                \
   } while(0)

#define DLLIST_ADDTAIL(__list, __item)          \
   do {                                         \
      (__item)->next = (__list);                \
      (__item)->prev = (__list)->prev;          \
      (__list)->prev->next = (__item);          \
      (__list)->prev = (__item);                \
   } while(0)

#define DLLIST_ADDHEAD(__list, __item)          \
   do {                                         \
      (__item)->prev = (__list);                \
      (__item)->next = (__list)->next;          \
      (__list)->next->prev = (__item);          \
      (__list)->next = (__item);                \
   } while(0)

#define DLLIST_MERGE(__listA, __listB, ty)      \
   do {                                         \
      ty prevB = (__listB)->prev;               \
      (__listA)->prev->next = (__listB);        \
      (__listB)->prev->next = (__listA);        \
      (__listB)->prev = (__listA)->prev;        \
      (__listA)->prev = prevB;                  \
   } while(0)

#define DLLIST_FOR_EACH(list, it) \
   for (DLList::Iterator (it) = (list)->iterator(); !(it).end(); (it).next())

class DLList
{
public:
   class Item
   {
   public:
      Item(void *priv) : next(this), prev(this), data(priv) { }

   public:
      Item *next;
      Item *prev;
      void *data;
   };

   DLList() : head(0) { }
   ~DLList() { clear(); }

   inline void insertHead(void *data)
   {
      Item *item = new Item(data);

      assert(data);

      item->prev = &head;
      item->next = head.next;
      head.next->prev = item;
      head.next = item;
   }

   inline void insertTail(void *data)
   {
      Item *item = new Item(data);

      assert(data);

      DLLIST_ADDTAIL(&head, item);
   }

   inline void insert(void *data) { insertTail(data); }

   void clear();

   class Iterator : public ManipIterator
   {
   public:
      Iterator(Item *head, bool r) : rev(r), pos(r ? head->prev : head->next),
                                     term(head) { }

      virtual void next() { if (!end()) pos = rev ? pos->prev : pos->next; }
      virtual void *get() const { return pos->data; }
      virtual bool end() const { return pos == term; }

      // caution: if you're at end-2 and erase it, then do next, you're at end
      virtual void erase();
      virtual bool insert(void *data);

      // move item to a another list, no consistency with its iterators though
      void moveToList(DLList&);

   private:
      const bool rev;
      Item *pos;
      Item *term;

      friend class DLList;
   };

   inline void erase(Iterator& pos)
   {
      pos.erase();
   }

   Iterator iterator()
   {
      return Iterator(&head, false);
   }

   Iterator revIterator()
   {
      return Iterator(&head, true);
   }

private:
   Item head;
};

class Stack
{
public:
   class Item {
   public:
      union {
         void *p;
         int i;
         unsigned int u;
         float f;
         double d;
      } u;

      Item() { memset(&u, 0, sizeof(u)); }
   };
   
   Stack() : size(0), limit(0), array(0) { }
   ~Stack() { if (array) FREE(array); }

   inline void push(int i)          { Item data; data.u.i = i; push(data); }
   inline void push(unsigned int u) { Item data; data.u.u = u; push(data); }
   inline void push(void *p)        { Item data; data.u.p = p; push(data); }
   inline void push(float f)        { Item data; data.u.f = f; push(data); }

   inline void push(Item data)
   {
      if (size == limit)
         resize();
      array[size++] = data;
   }

   inline Item pop()
   {
      if (!size) {
         Item data;
         assert(0);
         return data;
      }
      return array[--size];
   }

   inline unsigned int getSize() { return size; }

   inline Item& peek() { assert(size); return array[size - 1]; }

   void clear(bool releaseStorage = false)
   {
      if (releaseStorage && array)
         FREE(array);
      size = limit = 0;
   }

   void moveTo(Stack&); // move all items to target (not like push(pop()))

private:
   void resize()
   {
         unsigned int sizeOld, sizeNew;

         sizeOld = limit * sizeof(Item);
         limit = MAX2(4, limit + limit);
         sizeNew = limit * sizeof(Item);

         array = (Item *)REALLOC(array, sizeOld, sizeNew);
   }

   unsigned int size;
   unsigned int limit;
   Item *array;
};

class ArrayList
{
public:
   ArrayList();
   ~ArrayList();

   inline void reserve(int count)
   {
      if (count > limit)
         reallocate(count);
   }

   void insert(void *item, int& id)
   {
      id = ids.getSize() ? ids.pop().u.i : size++;

      assert(MAX2(limit + limit, 4) > size);
      if (size > limit)
         reallocate(MAX2(4, limit + limit));

      array[id] = item;
   }

   inline void remove(int& id)
   {
      assert(id < size && array[id]);

      ids.push(id);
      array[id] = 0;
      id = 0;
   }

   inline int getSize() { return size; }

   inline void *get(int id) { assert(id >= 0 && id < size); return array[id]; }

   inline void *operator[](int id) { return get(id); }

   class Iterator : public nv50_ir::Iterator
   {
   public:
      Iterator(ArrayList *array) : pos(0)
      {
         size = array->getSize();
         data = array->array;
         if (data)
            nextValid();
      }

      void nextValid() { while ((pos < size) && !data[pos]) ++pos; }

      void next() { if (pos < size) { ++pos; nextValid(); } }
      void *get() const { assert(pos < size); return data[pos]; }
      bool end() const { return pos >= size; }

   private:
      int pos;
      int size;
      void **data;

      friend class ArrayList;
   };

   Iterator iterator() { return Iterator(this); }

private:
   void reallocate(int capacity);

   int size;
   int limit;
   Stack ids;
   void **array;
};

class Interval
{
public:
   Interval() : head(0), tail(0) { }
   ~Interval();

   bool extend(int, int);
   void unify(Interval&); // clears source interval

   inline int begin() { return head ? head->bgn : -1; }
   inline int end() { checkTail(); return tail ? tail->end : -1; }
   inline bool isEmpty() { return !head; }
   bool overlaps(const Interval&) const;
   bool contains(int pos);

   void print() const;

   inline void checkTail() const;

private:
   class Range
   {
   public:
      Range(int a, int b) : next(0), bgn(a), end(b) { }

      Range *next;
      int bgn;
      int end;

      void coalesce(Range **ptail)
      {
         Range *rnn;

         while (next && end >= next->bgn) {
            assert(bgn <= next->bgn);
            rnn = next->next;
            end = MAX2(end, next->end);
            delete next;
            next = rnn;
         }
         if (!next)
            *ptail = this;
      }
   };

   Range *head;
   Range *tail;
};

class BitSet
{
public:
   BitSet() : marker(false), data(0), size(0) { }
   BitSet(unsigned int nBits, bool zero) : marker(false), data(0), size(0)
   {
      allocate(nBits, zero);
   }
   ~BitSet()
   {
      if (data)
         FREE(data);
   }

   bool allocate(unsigned int nBits, bool zero);

   inline unsigned int getSize() const { return size; }

   void fill(uint32_t val);

   void setOr(BitSet *, BitSet *); // second BitSet may be NULL

   inline void set(unsigned int i)
   {
      assert(i < size);
      data[i / 32] |= 1 << (i % 32);
   }

   inline void clr(unsigned int i)
   {
      assert(i < size);
      data[i / 32] &= ~(1 << (i % 32));
   }

   inline bool test(unsigned int i) const
   {
      assert(i < size);
      return data[i / 32] & (1 << (i % 32));
   }

   BitSet& operator|=(const BitSet&);

   BitSet& operator=(const BitSet& set)
   {
      assert(data && set.data);
      assert(size == set.size);
      memcpy(data, set.data, set.size / 8);
      return *this;
   }

   void andNot(const BitSet&);

   unsigned int popCount() const;

   void print() const;

public:
   bool marker; // for user

private:
   uint32_t *data;
   unsigned int size;
};

void Interval::checkTail() const
{
#if NV50_DEBUG & NV50_DEBUG_PROG_RA
   Range *r = head;
   while (r->next)
      r = r->next;
   assert(tail == r);
#endif
}

class MemoryPool
{
private:
   inline bool enlargeAllocationsArray(const unsigned int id, unsigned int nr)
   {
      const unsigned int size = sizeof(uint8_t *) * id;
      const unsigned int incr = sizeof(uint8_t *) * nr;

      uint8_t **alloc = (uint8_t **)REALLOC(allocArray, size, size + incr);
      if (!alloc)
         return false;
      allocArray = alloc;
      return true;
   }

   inline bool enlargeCapacity()
   {
      const unsigned int id = count >> objStepLog2;

      uint8_t *const mem = (uint8_t *)MALLOC(objSize << objStepLog2);
      if (!mem)
         return false;

      if (!(id % 32)) {
         if (!enlargeAllocationsArray(id, 32)) {
            FREE(mem);
            return false;
         }
      }
      allocArray[id] = mem;
      return true;
   }

public:
   MemoryPool(unsigned int size, unsigned int incr) : objSize(size),
                                                      objStepLog2(incr)
   {
      allocArray = NULL;
      released = NULL;
      count = 0;
   }

   ~MemoryPool()
   {
      for (unsigned i = 0; i < (count >> objStepLog2) && allocArray[i]; ++i)
         FREE(allocArray[i]);
      if (allocArray)
         FREE(allocArray);
   }

   void *allocate()
   {
      void *ret;
      const unsigned int mask = (1 << objStepLog2) - 1;

      if (released) {
         ret = released;
         released = *(void **)released;
         INFO("allocate(%u * %u = %u KiB): %p\n",
              count, objSize, (count * objSize) >> 10, ret);
         return ret;
      }

      if (!(count & mask))
         if (!enlargeCapacity())
            return NULL;

      ret = allocArray[count >> objStepLog2] + (count & mask) * objSize;
      ++count;
      INFO("allocate(%u * %u = %u KiB): %p\n",
           count, objSize, (count * objSize) >> 10, ret);
      return ret;
   }

   void release(void *ptr)
   {
      *(void **)ptr = released;
      released = ptr;
   }

private:
   uint8_t **allocArray; // array (list) of MALLOC allocations

   void *released; // list of released objects

   unsigned int count; // highest allocated object

   const unsigned int objSize;
   const unsigned int objStepLog2;
};

} // namespace nv50_ir

#endif // __NV50_IR_UTIL_H__