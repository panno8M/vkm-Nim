# Type //===================================================================== #

type
  Normalized*[T] = object
    unwraped*: T

# =====================================================================// Type #
# Converter //================================================================ #

converter unwrap*[T](x: Normalized[T]): lent T = x.unwraped
template `\`*[T](x: Normalized[T]): lent T = x.unwraped
func asNormalized*[T](x: T): Normalized[T] = Normalized[T](unwraped: x)
func normalize*[T](x: T): Normalized[T] =
  let norm = x.norm
  if norm == 0:
    raise newException(DivByZeroDefect, "cannot convert to normalized vector because length of the vector is zero")
  asNormalized(x/norm)
template `/!`*[T](x: T): lent Normalized[T] = asNormalized x
template `//`*[T](x: T): lent Normalized[T] = normalize x

func norm*[T](x: Normalized[T]) {.error.}
func norm2*[T](x: Normalized[T]) {.error.}

func `$`*[T](x: Normalized[T]) : string =
  "|" & $x.unwraped & "|==1"


# ================================================================// Converter #