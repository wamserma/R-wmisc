#ifdef WIN32         // means WIN64, too
#undef Realloc
#undef Free
#include <windows.h>
#include <stdio.h>
#include <tchar.h>
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>   // for open()
#include <unistd.h>  // for close()
#endif

#undef ERROR
#include <Rcpp.h>
#include <stdint.h>
#include <string.h>

using namespace Rcpp;
using namespace std;

//split a 64-bit pointer into two 32-bit values and store at given locations
__inline__ 
void unpackPtr(uint32_t* h,uint32_t* l,void* p){
  if (sizeof(char*) > sizeof(int)) { //64-bit platform
    *h = (uint32_t)((uint64_t)p>>32);
  } else { // 32-bit platform
    *h = 0;
  }
  *l = (uint32_t)(((uint64_t)p) & 0xFFFFFFFF);  
}

//split a 64-bit pointer into two 32-bit values and return intVector
__inline__ 
IntegerVector unpackPtrV(void* p){
  if (sizeof(char*) > sizeof(int)) { //64-bit platform
    return IntegerVector::create((uint32_t)((uint64_t)p>>32),(uint32_t)(((uint64_t)p) & 0xFFFFFFFF));
  } else { // 32-bit platform
    return IntegerVector::create(0,(uint32_t)(((uint64_t)p) & 0xFFFFFFFF));
  }
}


//take two 32-bit values and recombine into a 64-bit pointer value
__inline__ 
void* packPtr(uint32_t h,uint32_t l){
  if (sizeof(char*) > sizeof(int)) { //64-bit platform
    return (void *) ((((uint64_t)(h))<<32) | (uint64_t)(l));
  } else { // 32-bit platform
    return (void *)((uintptr_t)(l));  
  }  
}

// mmap a file and return an R-List with the created handles/pointers, unpacked into R's 32-bit ints
//[[Rcpp::export]]
List CWmisc_mmap(std::string path) {
  
  char* map;
  Function warning("warning");
  
#ifndef WIN32
  struct stat file_info;
  
  int fd = open( path.c_str(), O_RDONLY );
  if (fstat(fd, &file_info) == -1) {
    warning("File not found."); 
    List output = List::get_na();                
    return output;                               
  }
  int sz = file_info.st_size;
  if (sz <= 0) {
    warning("File is empty."); 
    List output = List::get_na();                
    return output;                               
  }
  #ifdef MAP_POPULATE
    map = (char*) mmap(0, sz, PROT_READ, MAP_SHARED | MAP_POPULATE, fd, 0);
  #else
    map = (char*) mmap(0, sz, PROT_READ, MAP_SHARED, fd, 0);
  #endif
  
  if (map == MAP_FAILED) {
    close(fd);                                    // # nocov
    warning("Error mapping the file.");           // # nocov
    List output = List::get_na();                 // # nocov
    return output;                                // # nocov
  }
  
#if _POSIX_C_SOURCE >= 200112L  
  posix_madvise(map, sz, POSIX_MADV_SEQUENTIAL);  // tell the OS we will access sequentially
                                                  // we don't care for a returned error
                                                  // see man7.org/linux/man-pages/man3/posix_madvise.3.html
#endif
  uint32_t map_h;                                                
  uint32_t map_l;
  
  unpackPtr(&map_h,&map_l,map);
  
  List output = List::create(Rcpp::Named("fd") = fd,
                             Rcpp::Named("sz") = sz,
                             Rcpp::Named("map") = IntegerVector::create(map_h,map_l));
  
  return output;
  
#else // Platform == Win32/64
  // tactlessly borrowed from data.table
  // Following: http://msdn.microsoft.com/en-gb/library/windows/desktop/aa366548(v=vs.85).aspx
  HANDLE hFile=0;
  HANDLE hMap=0;
  DWORD dwFileSize=0;
  const char* fnam = path.c_str();
  hFile = CreateFile(fnam, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL);
  if (hFile==INVALID_HANDLE_VALUE) {
    warning("File not found: ",fnam); 
    List output = List::get_na();     
    return output;                    
  }
  dwFileSize=GetFileSize(hFile,NULL);
  if (dwFileSize==0) { 
    CloseHandle(hFile);               
    warning("File is empty: ", fnam); 
    List output = List::get_na();     
    return output;                    
  }
  hMap=CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL); // dwFileSize+1 not allowed here, unlike mmap where +1 is zeroed
  if (hMap==NULL) { 
    CloseHandle(hFile);                  // # nocov start because this case is hard to produce in a test
    warning("This is Windows, CreateFileMapping returned error ", GetLastError(), " for file ", fnam);
    List output = List::get_na();         
    return output;                       // # nocov end
  }
  map = (char *)MapViewOfFile(hMap,FILE_MAP_READ,0,0,dwFileSize);
  if (map == NULL) {
    CloseHandle(hMap);                          // # nocov start because this case is hard to produce in a test
    CloseHandle(hFile);                         
    warning("Mapping file ", fnam, " failed."); 
    List output = List::get_na();               // # nocov end
    return output;
  }
  
  uint32_t hFile_h,hMap_h,map_h;
  uint32_t hFile_l,hMap_l,map_l;
  
  unpackPtr(&map_h,&map_l,map);
  unpackPtr(&hFile_h,&hFile_l,hFile);
  unpackPtr(&hMap_h,&hMap_l,hMap);
  
  List output = List::create(Rcpp::Named("hFile") = IntegerVector::create(hFile_h,hFile_l),
                             Rcpp::Named("hMap") = IntegerVector::create(hMap_h,hMap_l),
                             Rcpp::Named("sz") = dwFileSize, //DWORD is always 32bit https://msdn.microsoft.com/en-us/library/aa383751(VS.85).aspx
                             Rcpp::Named("map") = IntegerVector::create(map_h,map_l));
                               
  
  return output;
#endif
}  

// unmap a file opened as above and properly close it
//[[Rcpp::export]]
void CWmisc_munmap(List fdVec) {

  Rcpp::List fdlist(fdVec);
  
#ifndef WIN32
  int fd = as<int>(fdlist["fd"]);
  int sz = as<int>(fdlist["sz"]);
  IntegerVector mapVec = as<IntegerVector>(fdlist["map"]);
  char* map = (char*)packPtr(mapVec[0],mapVec[1]);
  
  munmap(map, sz);
  close(fd);
#else
  
  IntegerVector hFileVec = as<IntegerVector>(fdlist["hFile"]);
  HANDLE hFile = (HANDLE)packPtr(hFileVec[0],hFileVec[1]);
  
  IntegerVector hMapVec = as<IntegerVector>(fdlist["hMap"]);
  HANDLE hMap = (HANDLE)packPtr(hMapVec[0],hMapVec[1]);
  
  IntegerVector mapVec = as<IntegerVector>(fdlist["map"]);
  char* map = (char*)packPtr(mapVec[0],mapVec[1]);
  
  UnmapViewOfFile(map);
  CloseHandle(hMap);
  CloseHandle(hFile);
#endif
  
}

// read tokens from the mmap'd files, pretty much as what strtok does, also allowing a change of delimiters for every call
//[[Rcpp::export]]
List CWmisc_nextToken(IntegerVector currentPtr, IntegerVector delims){
  
  char* start = (char*)packPtr(currentPtr[0],currentPtr[1]);
  
  char *d = new char[delims.size()+1];

  // copy the delimiters from the IntegerVector to a char array usable by strcspn
  for (int i=0;i<delims.size();i++) d[i] = (char)delims[i];
  d[delims.size()] = 0; // strcspn expects \NULL-terminated strings
  
  size_t length = strcspn(start, d);
  
  delete[] d;
  // create the return value and copy the token
  
  List output = List::create(Rcpp::Named("token") = string(start, length),
                             Rcpp::Named("ptr") = unpackPtrV(start+length+1));
  return output;
}

// check whether target is in valid range, that is base <= target < base + offset
//[[Rcpp::export]]
bool CWmisc_validPtr(IntegerVector base, IntegerVector target, int offset){
  
  uint64_t start = (uint64_t)packPtr(base[0],base[1]);
  uint64_t ptr = (uint64_t)packPtr(target[0],target[1]);
  uint64_t end;
  
  if (ptr < start) return false;

#if (__GNUC__ > 4) && (__GNUC__ < 7)
 //if (__builtin_uaddl_overflow (start,offset, &end)) //assumes uint64_t aka unsigned long int # nolint
 if (__builtin_add_overflow (start,offset, &end))
   stop("Overflow in address computation.");
#elif __GNUC__ > 6  
  // error on overflow; builtins available for GCC >= 7
  if (__builtin_add_overflow_p (start,offset, (uint64_t) 0))
    stop("Overflow in address computation.");
  else
    end = start + offset;
#else
    end = start + offset;
#endif  
  if (ptr > end) return false;
  return true;
}

// pointer arithmetic
//[[Rcpp::export]]
IntegerVector CWmisc_subPtr(IntegerVector a, IntegerVector b){
  
  uint64_t start = (uint64_t)packPtr(a[0],a[1]);
  uint64_t offset = (uint64_t)packPtr(b[0],b[1]);
  uint64_t end;
  
#if (__GNUC__ > 4) && (__GNUC__ < 7)
  //if (__builtin_usubl_overflow (start,offset, &end)) //assumes uint64_t aka unsigned long int # nolint
  if (__builtin_sub_overflow (start,offset, &end))
    stop("Overflow in address computation.");
#elif __GNUC__ > 6  
  // error on overflow; builtins available for GCC >= 7
  if (__builtin_sub_overflow_p (start,offset, (uint64_t) 0))
    stop("Overflow in address computation.");
  else
    end = start - offset;
#else
  end = start - offset;
#endif 
return(unpackPtrV((void *)end));
}

//[[Rcpp::export]]
IntegerVector CWmisc_addPtr(IntegerVector a, IntegerVector b){
  
  uint64_t start = (uint64_t)packPtr(a[0],a[1]);
  uint64_t offset = (uint64_t)packPtr(b[0],b[1]);
  uint64_t end;
  
#if (__GNUC__ > 4) && (__GNUC__ < 7)
  //if (__builtin_uaddl_overflow (start,offset, &end)) //assumes uint64_t aka unsigned long int # nolint
  if (__builtin_add_overflow (start,offset, &end))
    stop("Overflow in address computation.");
#elif __GNUC__ > 6  
  // error on overflow; builtins available for GCC >= 7
  if (__builtin_add_overflow_p (start,offset, (uint64_t) 0))
    stop("Overflow in address computation.");
  else
    end = start + offset;
#else
  end = start + offset;
#endif 
  return(unpackPtrV((void *)end));
}
