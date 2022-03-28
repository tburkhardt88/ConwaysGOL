patterns <- list(
  cloverleaf = function(size){
    mid <- round(size/2)
    matrix(c(
      mid-3,mid-2,mid+2,mid+3,  mid-4,mid-1,mid+1,mid+4,  mid-4,mid-2,mid-1,mid+1,mid+2,mid+4,  mid - 5,mid-4,mid+4,mid + 5,    mid-3,mid-1,mid+1,mid+3,
      mid - 5,mid-4,mid+4,mid + 5,  mid-4,mid-2,mid-1,mid+1,mid+2,mid+4,  mid-4,mid-1,mid+1,mid+4,    mid-3,mid-2,mid+2,mid+3,# set the rows
      rep(mid-4, 4),   rep(mid-3,4),    rep(mid-2,6),          rep(mid-1,4),      rep(mid,4),
      rep(mid+1, 4),   rep(mid+2,6),          rep(mid+3,4),          rep(mid+4,4)      #set the cols
    ), ncol = 2)
  },
  f_pentomino = function(size){
    mid <- round(size/2)
    matrix(
      c(
        mid,  mid-1,mid,mid+1,mid-1,
        mid-1,mid,  mid,mid,  mid+1
      ),
      ncol = 2
    )
  },
  glider_gun = function(size){
    mid <- round(size/2)
    matrix(c(
      mid,   mid+1, mid,   mid+1, mid:(mid+2),  mid-1,mid+3,mid-2,mid+4,mid-2,mid+4,mid+1,mid-1,mid+3,mid:(mid+2), mid+1,
      (mid-2):mid, (mid-2):mid, mid-3,mid+1,mid-4,mid-3,mid+1,mid+2,mid-2,mid-1,  mid-2,mid-1,
      mid-19,mid-19,mid-18,mid-18,rep(mid-9, 3),mid-8,mid-8,mid-7,mid-7,mid-6,mid-6,mid-5,mid-4,mid-4,rep(mid-3,3),mid-2,
      rep(mid+1,3),rep(mid+2,3),mid+3,mid+3,rep(mid+5,4),           rep(mid+15,2),rep(mid+16,2) 
    ), ncol = 2)
  }
)

