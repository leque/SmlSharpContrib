structure LazyList = struct
  datatype 'a node = Nil | Cons of 'a * 'a t
  withtype 'a t = 'a node Susp.t

  fun next xs = Susp.force xs

  fun nil_ () = Susp.fromVal Nil

  fun cons (x, xs) =
      Susp.delay (fn () => Cons (x, xs))

  fun null xs =
      case next xs of
          Nil => true
        | Cons (_, _) => false

  fun foldl f init xs =
      Susp.lazy (fn () =>
                    case next xs of
                        Nil => init
                      | Cons (x, xs') =>
                        foldl f (f (x, init)) xs')

  fun foldl' f init xs =
      case next xs of
          Nil => init
        | Cons (x, xs') => foldl' f (f (x, init)) xs'

  fun foldr f init xs =
      Susp.lazy (fn () =>
                    case next xs of
                        Nil => init
                      | Cons (x, xs') => f (x, foldr f init xs'))

  fun foldr' f init xs =
      case next xs of
          Nil => init
        | Cons (x, xs') => f (x, foldr' f init xs')

  fun hd xs =
      case next xs of
          Nil => NONE
        | Cons (x, _) => SOME x

  fun tl xs =
      case next xs of
          Nil => NONE
        | Cons (_, xs') => SOME xs'

  fun length xs =
      foldl' (fn (_, n) => n + 1) 0 xs

  fun last xs =
      foldl' (fn (x, _) => SOME x) NONE xs

  fun getItem xs =
      case next xs of
          Nil => NONE
        | Cons (x, xs') => SOME (x, xs')

  fun nth (xs, i) =
      if i < 0 then
        NONE
      else
        case next xs of
            Nil => NONE
          | Cons (x, xs') =>
            if i = 0 then
              SOME x
            else
              nth (xs', i - 1)

  fun take (xs, n) =
      Susp.lazy
          (fn () =>
              if n <= 0 then
                nil_ ()
              else
                case next xs of
                    Nil => nil_ ()
                  | Cons (x, xs') => cons (x, take (xs', n - 1)))

  fun drop (xs, n) =
      Susp.lazy
          (fn () =>
              if n <= 0 then
                xs
              else
                case next xs of
                    Nil => nil_ ()
                  | Cons (_, xs') => drop (xs', n - 1))

  fun append (xs, ys) =
      Susp.lazy (fn () => foldr cons ys xs)

  fun revAppend (xs, ys) =
      Susp.lazy (fn () => foldl cons ys xs)

  fun rev xs =
      Susp.lazy (fn () => revAppend (xs, nil_ ()))

  fun concat xss =
      Susp.lazy (fn () => foldr append (nil_ ()) xss)

  fun app f xs =
      case next xs of
          Nil => ()
        | Cons (x, xs') =>
          let
            val () = f x
          in
            app f xs'
          end

  fun map f xs =
      Susp.lazy
          (fn () =>
              case next xs of
                  Nil => nil_ ()
                | Cons (x, xs') => cons (f x, map f xs'))

  fun mapPartial f xs =
      Susp.lazy
          (fn () =>
              case next xs of
                  Nil => nil_ ()
                | Cons (x, xs') =>
                  case f x of
                      NONE => mapPartial f xs'
                    | SOME v => cons (v, mapPartial f xs'))

  fun find p xs =
      case next xs of
          Nil => NONE
        | Cons (x, xs') =>
          if p x then
            SOME x
          else
            find p xs'

  fun filter p xs =
      Susp.lazy
          (fn () =>
              case next xs of
                  Nil => nil_ ()
                | Cons (x, xs') =>
                  if p x then
                    cons (x, filter p xs')
                  else
                    filter p xs')

  fun partition p xs =
      let
        open Either
        val vs = map (fn x => if p x then Left x else Right x) xs
      in
        (mapPartial getLeft vs, mapPartial getRight vs)
      end

  fun exists p xs =
      case next xs of
          Nil => false
        | Cons (x, xs') =>
          p x orelse exists p xs'

  fun all p xs =
      case next xs of
          Nil => true
        | Cons (x, xs') =>
          p x andalso all p xs'

  fun tabulate (n, f) =
      let
        fun loop m =
            Susp.lazy (fn () =>
                          if m = 0 then
                            nil_ ()
                          else
                            cons (f (n - m), loop (m - 1)))
      in
        if n <= 0 then
          nil_ ()
        else
          loop n
      end

  fun collate f (xs, ys) =
      case (next xs, next ys) of
          (Nil, Nil) => EQUAL
        | (Nil, Cons (_, _)) => LESS
        | (Cons (_, _), Nil) => GREATER
        | (Cons (x, xs'), Cons (y, ys')) =>
          case f (x, y) of
              LESS => LESS
            | GREATER => GREATER
            | EQUAL => collate f (xs', ys')

  fun intersperse (sep, xs) =
      Susp.lazy
          (fn () =>
              case next xs of
                  Nil => nil_ ()
                | Cons (x, xs') => cons (x, cons (sep, intersperse (sep, xs'))))

  fun intercalate (xs, yss) =
      Susp.lazy (fn () => concat (intersperse (xs, yss)))

  fun iterate f init =
      Susp.lazy (fn () => cons (init, iterate f (f init)))

  fun repeat x =
      let
        val xs = ref (nil_ ())
        fun f () = Susp.lazy (fn () => cons (x, !xs))
      in
        xs := f ();
        !xs
      end

  fun replicate (n, x) =
      take (repeat x, n)

  fun cycle xs =
      let
        val ys = ref (nil_ ())
        fun f () = Susp.lazy (fn () => append (xs, !ys))
      in
        ys := f();
        !ys
      end

  fun unfoldr f init =
      Susp.lazy
          (fn () =>
              case f init of
                  SOME (x, init') => cons (x, unfoldr f init')
                | NONE => nil_ ())

  fun concatMap f xs =
      concat (map f xs)

  fun splitAt (xs, n) =
      (take (xs, n), drop (xs, n))

  fun takeWhile p xs =
      Susp.lazy
          (fn () =>
              case next xs of
                  Nil => nil_ ()
                | Cons (x, xs') =>
                  if p x then
                    cons (x, takeWhile p xs')
                  else
                    nil_ ())

  fun dropWhile p xs =
      Susp.lazy
          (fn () =>
              case next xs of
                  Nil => nil_ ()
                | Cons (x, xs') =>
                  if p x then
                    dropWhile p xs'
                  else
                    xs)

  fun span p xs =
      (takeWhile p xs, dropWhile p xs)

  fun break p xs =
      span (not o p) xs

  fun lengthGreaterThan (xs, n) =
      if n < 0 then
        true
      else
        case next xs of
            Nil => false
          | Cons (_, xs') => lengthGreaterThan (xs', n - 1)

  fun zipWith f (xs, ys) =
      Susp.lazy
          (fn () =>
              case (next xs, next ys) of
                  (Nil, _) => nil_ ()
                | (_, Nil) => nil_ ()
                | (Cons (x, xs'), Cons (y, ys')) =>
                  cons (f (x, y), zipWith f (xs', ys')))

  fun zip (xs, ys) =
      zipWith (fn x => x) (xs, ys)

  fun unzip xs =
      (map #1 xs, map #2 xs)

  fun fromList [] = Susp.lazy (fn () => nil_ ())
    | fromList (x :: xs) = Susp.lazy (fn () => cons (x, fromList xs))

  fun toList xs =
      case next xs of
          Nil => []
        | Cons (x, xs') => x :: toList xs'
end
