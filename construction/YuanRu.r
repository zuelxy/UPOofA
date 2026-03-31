
YuanRu <- function(N) {
  # 检查输入参数
  if (N <= 2) {
    stop("N must be greater than 2")
  }
  
  # 步骤1: 生成生成器向量 h
  h <- generate_generator_vector(N)
  #n <- length(h)
  
  # 步骤2: 生成拉丁方 L
  L <- generate_latin_square(h, N)
  
  # 步骤3: 将 h_i 替换为 i，得到 LHD
  D <- replace_with_indices(L, h)
  
  return(D)
}

#' 生成生成器向量 h
#'
#' 根据公式(3.1)生成生成器向量 h
#'
#' @param N 正整数
#' @return 生成器向量 h
generate_generator_vector <- function(N) {
  # 计算欧拉函数 φ(N)
  phi_N <- euler_phi(N)
  
  # n = φ(N)/2
  n <- phi_N / 2
  
  # 找到所有小于等于 N/2 且与 N 互质的正整数
  candidates <- 1:floor(N/2)
  coprime_candidates <- candidates[sapply(candidates, function(x) gcd(x, N) == 1)]
  
  # 取前 n 个元素作为生成器向量
  h <- coprime_candidates[1:n]
  
  return(h)
}

#' 计算欧拉函数 φ(N)
#'
#' 计算小于等于 N 且与 N 互质的正整数个数
#'
#' @param N 正整数
#' @return φ(N)
euler_phi <- function(N) {
  if (N == 1) return(1)
  
  # 质因数分解
  factors <- prime_factors(N)
  unique_factors <- unique(factors)
  
  # 计算 φ(N) = N * ∏(1 - 1/p)
  result <- N
  for (p in unique_factors) {
    result <- result * (1 - 1/p)
  }
  
  return(round(result))
}

#' 计算最大公约数
#'
#' 使用欧几里得算法计算两个数的最大公约数
#'
#' @param a 正整数
#' @param b 正整数
#' @return a 和 b 的最大公约数
gcd <- function(a, b) {
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(a)
}

#' 质因数分解
#'
#' 计算一个数的质因数分解
#'
#' @param n 正整数
#' @return 包含所有质因数的向量
prime_factors <- function(n) {
  factors <- c()
  
  # 处理因子2
  while (n %% 2 == 0) {
    factors <- c(factors, 2)
    n <- n / 2
  }
  
  # 处理奇数因子
  i <- 3
  while (i <= sqrt(n)) {
    while (n %% i == 0) {
      factors <- c(factors, i)
      n <- n / i
    }
    i <- i + 2
  }
  
  # 如果 n 是大于2的质数
  if (n > 2) {
    factors <- c(factors, n)
  }
  
  return(factors)
}

#' 生成拉丁方 L
#'
#' 根据公式(3.2)生成拉丁方 L
#'
#' @param h 生成器向量
#' @param N 正整数
#' @return n × n 的拉丁方矩阵
generate_latin_square <- function(h, N) {
  n <- length(h)
  L <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      # 计算 h_i * h_j (mod N)
      product_mod <- (h[i] * h[j]) %% N
      
      # 计算 r_{ij} = min{product_mod, N - product_mod}
      r_ij <- min(product_mod, N - product_mod)
      
      # 如果结果为0，则取N/2（根据论文描述，h_i ≤ N/2）
      if (r_ij == 0) {
        r_ij <- N / 2
      }
      
      L[i, j] <- r_ij
    }
  }
  
  return(L)
}

#' 将 h_i 替换为索引 i
#'
#' 将拉丁方 L 中的每个元素 h_i 替换为它的索引 i
#'
#' @param L 拉丁方矩阵
#' @param h 生成器向量
#' @return 替换后的拉丁超立方设计
replace_with_indices <- function(L, h) {
  n <- length(h)
  D <- matrix(0, n, n)
  
  # 创建映射：h_i -> i
  mapping <- 1:n
  names(mapping) <- as.character(h)
  
  # 替换矩阵中的元素
  for (i in 1:n) {
    for (j in 1:n) {
      # 找到 h_i 在向量 h 中的位置
      value <- L[i, j]
      index <- which(h == value)
      
      # 如果找到，替换为索引
      if (length(index) > 0) {
        D[i, j] <- index
      } else {
        # 如果找不到精确匹配，找到最接近的值
        closest_index <- which.min(abs(h - value))
        D[i, j] <- closest_index
      }
    }
  }
  
  return(D)
}

