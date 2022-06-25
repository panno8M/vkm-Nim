import ./normals
import ./vector_basic

# Math //===================================================================== #

{.push, inline.}
func `-`*[N; T](v: Normalized[array[N,T]]): Normalized[array[N,T]] = asNormalized -v.unwrap
template `+`*[N; T](v: Normalized[array[N,T]]): Normalized[array[N,T]] = v

func cross*[T](a, b: Normalized[array[3,T]]): Normalized[array[3,T]] = asNormalized cross(a.unwrap, b.unwrap)

func reflect*[N: static int; T](ray: array[N, T]; n: Normalized[array[N,T]]): array[N,T] =
  ## For a given incident vector ``i`` and surface normal ``n`` reflect returns the reflection direction
  ray + (-2*dot(n,ray))*n
{.pop.}


# =====================================================================// Math #