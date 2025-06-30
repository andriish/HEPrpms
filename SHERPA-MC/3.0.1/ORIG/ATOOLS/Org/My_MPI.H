#ifndef ATOOLS__Org__My_MPI_H
#define ATOOLS__Org__My_MPI_H

#include "ATOOLS/Org/CXXFLAGS.H"

#ifdef USING__MPI
#include "mpi.h"
#include <string>
#endif

#ifdef USING__Threading
#include <pthread.h>
inline int pthread_cond_signal
(pthread_cond_t *c,pthread_mutex_t *m)
{
  pthread_mutex_lock(m);
  int r(pthread_cond_signal(c));
  pthread_mutex_unlock(m);
  return r;
}
#endif

#include <vector>

namespace ATOOLS {

  class My_MPI {

  public:

    My_MPI();

    void PrintRankInfo();
    void PrintRank();

#ifdef USING__MPI

    void Barrier() {
      MPI_Barrier(m_comm);
    }

    int Rank() {
      int rank;
      MPI_Comm_rank(m_comm, &rank);
      return rank;
    }

    int Size() {
      int size;
      MPI_Comm_size(m_comm, &size);
      return size;
    }

    void Bcast(void* buffer, int count, MPI_Datatype type) {
      MPI_Bcast(buffer, count, type, 0, m_comm);
    }

    void Reduce(void* buffer, int count, MPI_Datatype type, MPI_Op op) {
      if (Rank()==0)
        MPI_Reduce(MPI_IN_PLACE, buffer, count, type, op, 0, m_comm);
      else
        MPI_Reduce(buffer, buffer, count, type, op, 0, m_comm);
    }

    void Allreduce(void* buffer, int count, MPI_Datatype type, MPI_Op op) {
      MPI_Allreduce(MPI_IN_PLACE, buffer, count, type, op, m_comm);
    }

    void Allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
                         void *recvbuf, int recvcount, MPI_Datatype recvtype) {
      MPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype,
                    m_comm);
    }

    void Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag)
    {
      MPI_Recv(buf, count, datatype, source, tag, m_comm, MPI_STATUS_IGNORE);
    }

    int Send(const void *buf, int count, MPI_Datatype datatype, int dest,
             int tag)
    {
      return MPI_Send(buf, count, datatype, dest, tag, m_comm);
    }

    void Gather(void *sendbuf, int sendcount, MPI_Datatype sendtype,
		void *recvbuf, int recvcount, MPI_Datatype recvtype,
		int root)
    {
      MPI_Gather(sendbuf, sendcount, sendtype,
		 recvbuf, recvcount, recvtype, root, m_comm);
    }

    // Convenience method to get the maximum of some value from all ranks.
    int Allmax(int);

    // Convenience method to gather strings from all ranks into an array.
    // Allgather is used, such that the returned vector is filled for all
    // ranks. Note that this involves a non-trivial amount of communication
    // and should not be used too often.
    std::vector<std::string> AllgatherStrings(const std::string&);

#endif

  private:

#ifdef USING__MPI
    MPI_Comm m_comm;
#endif

  };// end of class My_MPI

  extern My_MPI* mpi;

  void Abort(const int mode=0);

}// end of namespace ATOOLS

#endif
