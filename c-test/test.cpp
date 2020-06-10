extern "C" {
#include <linux/io_uring.h>
#include <sys/uio.h>
#include <sys/epoll.h>
#include <fcntl.h>

#include "syscall.h"
#include "hs_uring.h"
}

#include <system_error>
#include <cstdint>
#include <cassert>
#include <iostream>
#include <functional>

inline void write_barrier() { __asm__ __volatile__("":::"memory"); }
inline void read_barrier() { __asm__ __volatile__("":::"memory"); }

void print_sqe(struct io_uring_sqe sqe) {
  printf("sqe.fd:        %d\n", sqe.fd);
  printf("sqe.flags:     0x%x\n", sqe.flags);
  printf("sqe.opcode:    %d\n", sqe.opcode);
  printf("sqe.addr:      0x%llx\n", sqe.addr);
  printf("sqe.len:       0x%x\n", sqe.len);
  printf("sqe.off:       0x%llx\n", sqe.off);
  printf("sqe.user_data: 0x%llx\n", sqe.user_data);
}

class URing {
  hs_uring *const uring;
  uint32_t *const sq_head;
  uint32_t *const sq_tail;
  uint32_t *const sq_ring_mask;
  uint32_t *const sq_array;

  uint32_t *const cq_head;
  uint32_t *const cq_tail;
  uint32_t *const cq_ring_mask;

  struct io_uring_cqe *const cqes;

  struct io_uring_sqe *const sqe_arr;

public:
#define APERT_OFF(aperture, offset_field) \
    ((uint32_t*) ((uint8_t*) uring->aperture + uring->params.offset_field))

  URing(struct hs_uring *uring)
    : uring(uring),
      sq_head(APERT_OFF(sq_aperture, sq_off.head)),
      sq_tail(APERT_OFF(sq_aperture, sq_off.tail)),
      sq_ring_mask(APERT_OFF(sq_aperture, sq_off.ring_mask)),
      sq_array(APERT_OFF(sq_aperture, sq_off.array)),

      cq_head(APERT_OFF(cq_aperture, cq_off.head)),
      cq_tail(APERT_OFF(cq_aperture, cq_off.tail)),
      cq_ring_mask(APERT_OFF(cq_aperture, cq_off.ring_mask)),

      cqes((struct io_uring_cqe*) APERT_OFF(cq_aperture, cq_off.cqes)),

      sqe_arr((struct io_uring_sqe*)uring->sqe_aperture)
  {}
#undef APERT_OFF

  int fd() const {
    return this->uring->uring_fd;
  }

  void push_sqe(struct io_uring_sqe sqe) {
    const uint32_t tail = *this->sq_tail;
    const uint32_t index = tail & *this->sq_ring_mask;

    this->sqe_arr[index] = sqe;
    this->sq_array[index] = index;

    printf("pushed SQE %d:\n", index);
    print_sqe(this->sqe_arr[index]);

    write_barrier();
    *this->sq_tail = tail + 1;
    write_barrier();
  }

  int enter_wait(int to_submit, int min_complete) {
    return this->enter(to_submit, min_complete, IORING_ENTER_GETEVENTS);
  }

  int enter(int to_submit, int min_complete, int flags) {
    int ret = io_uring_enter(this->uring->uring_fd, to_submit, min_complete, flags, NULL);
    if (ret < 0) {
      std::error_code errcode(errno, std::system_category());
      throw std::system_error(errcode, "io_uring_enter");
    }
    return ret;
  }

  void check_cq(std::function<void(struct io_uring_cqe)> handle_cqe) {
    uint32_t head = *this->cq_head;
    while (1) {
      read_barrier();
      if (head == *this->cq_tail)
        break;

      uint32_t index = head & *this->cq_ring_mask;
      handle_cqe(this->cqes[index]);
      head++;
    }
  }
};

int main() {
  struct hs_uring *hs_uring = hs_new_uring(128);
  assert(hs_uring != NULL);
  URing uring(hs_uring);

  int fd = open("LICENSE", O_RDONLY);
  if (fd < 0) {
    std::error_code errcode(errno, std::system_category());
    throw std::system_error(errcode, "io_uring_enter");
  }

  char buf[256];
  struct iovec iovecs[] = {
    { .iov_base = buf, .iov_len = 128 },
  };
  struct io_uring_sqe sqe {
    .opcode = IORING_OP_READV,
    .flags = 0,
    .fd = fd,
    .off = 0,
    .addr = (uint64_t) iovecs,
    .len = 1,
    .user_data = 0xdeadbeef
  };
  uring.push_sqe(sqe);
  int ret = 0;
  ret = uring.enter(1, 0, 0);

  int epoll_fd = epoll_create(16);
  struct epoll_event events[1] = { EPOLLIN, (void*) 42 };
  if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, uring.fd(), events) != 0) {
    std::error_code errcode(errno, std::system_category());
    throw std::system_error(errcode, "io_uring_enter");
  }
  ret = epoll_wait(epoll_fd, events, 1, -1);
  if (ret == -1) {
    std::error_code errcode(errno, std::system_category());
    throw std::system_error(errcode, "io_uring_enter");
  } else {
    std::cout << "epoll completed: " << events[0].data.u64 << std::endl;
  }
  std::cout << "RET: " << ret << std::endl;

  printf("%d\n", ret);
  for (int i=0; i<128; i++)
    printf("%c", buf[i]);

  hs_free_uring(hs_uring);
  return 0;
}
