structure LazyListTest = struct
  open SMLUnit
  open Std
  structure L = LazyList

  fun push x xs =
      xs := x :: !xs

  val srand = _import "srand" : (int) -> ()
  val rand = _import "rand" : () -> int

  fun randList maxlen =
      let
        val len = rand () mod maxlen
      in
        List.tabulate (len, fn _ => rand ())
      end

  fun times n f =
      if n <= 0 then
        ()
      else (
        f ();
        times (n - 1) f
      )

  fun hdTest () =
      let
        val () = Assert.assertNone $ L.hd $ L.nil_ ()
        val v = 42
        val h = L.hd $ L.cons (v, L.nil_ ())
        val () = Assert.assertSome h
      in
        Assert.assertEqualIntOption (SOME v) h
      end

  fun lengthTest () =
      let
        val () = Assert.assertEqualInt 0 $ L.length $ L.nil_ ()
        val xs = L.cons (42, L.nil_ ())
        val () = Assert.assertEqualInt 1 $ L.length xs
        val xs' = L.cons (42, xs)
      in
        Assert.assertEqualInt (L.length xs + 1) $ L.length xs'
      end

  fun nullTest () =
      let in
        Assert.assertTrue $ L.null $ L.nil_ ();
        Assert.assertFalse $ L.null $ L.cons (42, L.nil_ ())
      end

  fun tlTest () =
      let
        val () = Assert.assertNone $ L.tl $ L.nil_ ()
        val xs = L.cons (42, L.nil_ ())
        val t = L.tl xs
        val () = Assert.assertSome t
        val t = Option.valOf t
      in
        Assert.assertEqualInt (L.length xs - 1) (L.length t);
        Assert.assertTrue $ L.null t
      end

  fun nthTest () =
      let
        val () = Assert.assertNone $ L.nth (L.nil_ (), 0)
        val xs = L.cons (0, L.cons (1, L.cons (2, L.nil_ ())))
        val () = Assert.assertEqualIntOption (SOME 0) $ L.nth (xs, 0)
        val () = Assert.assertEqualIntOption (SOME 1) $ L.nth (xs, 1)
        val () = Assert.assertEqualIntOption (SOME 2) $ L.nth (xs, 2)
      in
        Assert.assertNone $ L.nth (xs, 3)
      end

  fun fromListTest () =
      let
        val () = Assert.assertTrue $ L.null $ L.fromList []
        val lis = [0, 1, 2]
        val xs = L.fromList lis
        val () = Assert.assertEqualInt (List.length lis) (L.length xs)
        val () = Assert.assertEqualIntOption (SOME $ List.nth (lis, 0))
                                             $ L.nth (xs, 0)
        val () = Assert.assertEqualIntOption (SOME $ List.nth (lis, 1))
                                             $ L.nth (xs, 1)
      in
        Assert.assertEqualIntOption (SOME $ List.nth (lis, 2))
                                    $ L.nth (xs, 2)
      end

  fun toListTest () =
      let
        val () = Assert.assertTrue $ List.null $ L.toList $ L.nil_ ()
        val xs = L.cons (0, L.cons (1, L.cons (2, L.nil_ ())))
        val lis = L.toList xs
        val () = Assert.assertEqualInt (L.length xs) (List.length lis)
        val () = Assert.assertEqualIntOption (L.nth (xs, 0))
                                             $ SOME $ List.nth (lis, 0)
        val () = Assert.assertEqualIntOption (L.nth (xs, 1))
                                             $ SOME $ List.nth (lis, 1)
      in
        Assert.assertEqualIntOption (L.nth (xs, 2))
                                    $ SOME $ List.nth (lis, 2)
      end

  fun fromList_toListTest () =
      let
        fun test lis =
            Assert.assertEqualIntList lis
                                      $ L.toList $ L.fromList lis
      in
        times 30 (fn () => test (randList 10))
      end

  fun lastTest () =
      let
        val () = Assert.assertNone $ L.last $ L.nil_ ()
        fun test lis =
            Assert.assertEqualIntOption (SOME $ List.last lis)
                                        $ L.last $ L.fromList lis
      in
        times 10 (fn () => test (0 :: randList 10))
      end

  fun getItemTest () =
      let
        fun test lis =
            let
              val v = L.getItem $ L.fromList lis
              val lv = List.getItem lis
              val () = Assert.assertEqualBool
                           (Option.isSome lv)
                           (Option.isSome v)
              val () = Assert.assertEqualIntOption
                           (Option.map #1 lv)
                           $ Option.map #1 v
              val () =
                  if Option.isSome lv then
                    Assert.assertEqualIntList
                        (#2 $ Option.valOf lv)
                        $ (L.toList $ #2 $ Option.valOf v)
                  else ()
            in
              ()
            end
      in
        test [];
        times 10 (fn () => test (randList 10))
      end

  fun collateTest () =
      let
        fun col order xs ys =
            Assert.assertEqualOrder
                order $ L.collate Int.compare (L.fromList xs, L.fromList ys)
        val () = col EQUAL   [] []
        val () = col LESS    [] [1]
        val () = col GREATER [1] []
        val () = col EQUAL   [1] [1]
        val () = col LESS    [0] [1]
        val () = col GREATER [1] [0]
        val () = col EQUAL   [0, 1] [0, 1]
        val () = col LESS    [0, 1] [0, 1, 2]
        val () = col GREATER [0, 1, 2] [0, 1]
        fun test xs ys =
            Assert.assertEqualOrder
                (List.collate Int.compare (xs, ys))
                $ L.collate Int.compare (L.fromList xs, L.fromList ys)
      in
        times 10 (fn () => test (randList 10) (randList 10))
      end

  fun takeTest () =
      let
        val () = Assert.assertEqualInt 0 $ L.length $ L.take (L.nil_ (), 4)
        val lis = List.tabulate (5, id)
        val xs = L.fromList lis
        val () = Assert.assertEqualInt 3 $ L.length $ L.take (xs, 3)
        val () = Assert.assertEqualInt 5 $ L.length $ L.take (xs, 5)
        val () = Assert.assertEqualInt 5 $ L.length $ L.take (xs, 10)
        val () = Assert.assertEqualIntList (List.take (lis, 3))
                                           $ L.toList $ L.take (xs, 3)
        val () = Assert.assertEqualIntList (List.take (lis, 5))
                                           $ L.toList $ L.take (xs, 5)
        val () = Assert.assertEqualIntList (List.take (lis, 5))
                                           $ L.toList $ L.take (xs, 10)
      in
        ()
      end

  fun dropTest () =
      let
        val () = Assert.assertEqualInt 0 $ L.length $ L.drop (L.nil_ (), 4)
        val lis = List.tabulate (5, id)
        val xs = L.fromList lis
        val () = Assert.assertEqualInt 2 $ L.length $ L.drop (xs, 3)
        val () = Assert.assertEqualInt 0 $ L.length $ L.drop (xs, 5)
        val () = Assert.assertEqualInt 0 $ L.length $ L.drop (xs, 10)
        val () = Assert.assertEqualIntList (List.drop (lis, 3))
                                           $ L.toList $ L.drop (xs, 3)
        val () = Assert.assertEqualIntList (List.drop (lis, 5))
                                           $ L.toList $ L.drop (xs, 5)
        val () = Assert.assertEqualIntList (List.drop (lis, 5))
                                           $ L.toList $ L.drop (xs, 10)
      in
        ()
      end

  fun appendTest () =
      let
        fun test xs ys =
            Assert.assertEqualIntList
                (xs @ ys)
                $ L.toList
                $ L.append (L.fromList xs, L.fromList ys)
      in
        test [] [];
        times 3 (fn () => test (randList 5) []);
        times 3 (fn () => test [] (randList 5));
        times 3 (fn () => test (randList 5) (randList 5))
      end

  fun revAppendTest () =
      let
        fun test xs ys =
            Assert.assertEqualIntList
                (List.revAppend (xs, ys))
                $ L.toList
                $ L.revAppend (L.fromList xs, L.fromList ys)
      in
        test [] [];
        times 3 (fn () => test (randList 5) []);
        times 3 (fn () => test [] (randList 5));
        times 3 (fn () => test (randList 5) (randList 5))
      end

  fun revTest () =
      let
        fun test xs =
            Assert.assertEqualIntList
                (List.rev xs)
                $ L.toList
                $ L.rev
                $ L.fromList xs
      in
        test [];
        times 5 (fn () => test (randList 10))
      end

  fun appTest () =
      let
        fun test xs =
            let
              val buf = ref []
              fun f n = push n buf
            in
              L.app f $ L.fromList xs;
              Assert.assertEqualIntList (List.rev xs) $ !buf
            end
      in
        test [];
        times 5 (fn () => test (randList 10))
      end

  fun mapTest () =
      let
        fun f n = n * 2 - 1
        fun test xs =
            Assert.assertEqualIntList
                (List.map f xs)
                $ L.toList
                $ L.map f
                $ L.fromList xs
      in
        test [];
        times 5 (fn () => test (randList 10))
      end

  fun even n = n mod 2 = 0

  fun odd n = not $ even n

  fun mapPartialTest () =
      let
        fun f n =
            if even n then SOME (Int.quot (n, 2)) else NONE
        fun test xs =
            Assert.assertEqualIntList
                (List.mapPartial f xs)
                $ L.toList
                $ L.mapPartial f
                $ L.fromList xs
      in
        test [];
        times 5 (fn () => test (randList 10))
      end

  fun findTest () =
      let
        fun test p lis =
            Assert.assertEqualIntOption (List.find p lis)
                                        $ L.find p $ L.fromList lis
      in
        test (fn n => n = 2) [];
        times 5 (fn () => test (fn n => n = 5) (randList 10));
        times 5 (fn () => test (fn n => n = 11) (randList 10))
      end

  fun filterTest () =
      let
        fun test p lis =
            Assert.assertEqualIntList (List.filter p lis)
                                      $ L.toList $ L.filter p $ L.fromList lis
      in
        test even [];
        test odd [];
        times 5 (fn () => test even (randList 10));
        times 5 (fn () => test odd (randList 10))
      end

  fun partitionTest () =
      let
        fun test p lis =
            let
              val (l1, l2) = List.partition p lis
              val (xs, ys) = L.partition p $ L.fromList lis
            in
              Assert.assertEqualIntList l1 $ L.toList $ xs;
              Assert.assertEqualIntList l2 $ L.toList $ ys
            end
      in
        test even [];
        times 5 (fn () => test even (randList 10));
        times 5 (fn () => test odd (randList 10))
      end

  fun existsTest () =
      let
        fun test p lis =
            Assert.assertEqualBool (List.exists p lis)
                                   $ L.exists p $ L.fromList lis
      in
        test even [];
        times 5 (fn () => test (fn n => n = 5) (randList 10));
        times 5 (fn () => test (fn n => n = 11) (randList 10))
      end

  fun allTest () =
      let
        fun test p lis =
            Assert.assertEqualBool (List.all p lis)
                                   $ L.all p $ L.fromList lis
      in
        test even [];
        times 5 (fn () => test even (randList 10));
        times 5 (fn () => test (fn n => n < 11) (randList 10))
      end

  fun tabulateTest () =
      let
        fun test n f =
            Assert.assertEqualIntList
                (List.tabulate (n, f))
                $ L.toList $ L.tabulate (n, f)
      in
        times 5 (fn () => test (rand () mod 10) (fn n => n * 3 + 1));
        times 5 (fn () => test (rand () mod 10) (fn n => Int.quot (n, 3) + 1))
      end

  fun intersperseTest () =
      let
        fun test sep lis =
            let
              val xs = L.intersperse (sep, L.fromList lis)
              val len = List.length lis
            in
              List.app (fn n =>
                           ( Assert.assertEqualIntOption
                                 (SOME $ List.nth (lis, n))
                                 $ L.nth (xs, n * 2);
                             if n < len - 1 then
                               Assert.assertEqualIntOption
                                   (SOME sep)
                                   $ L.nth (xs, n * 2 + 1)
                             else
                               ()))
                       (List.tabulate (len, id))
            end
      in
        test ~1 [];
        times 10 (fn () => test ~1 $ randList 10)
      end

  (* ------------------------------------------------------------ *)

  fun makeTicks len =
      let
        fun succ n = n + 1
        val buf = ref []
        val xs = L.take (L.iterate (fn n => (push () buf; succ n)) 0, len)
        fun count () = List.length $ !buf
      in
        (count, xs)
      end

  fun nullCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.null xs
      in
        Assert.assertEqualInt 1 $ count ()
      end

  fun foldlCompTest () =
      let
        fun f (n, sum) = Susp.delay (fn () => n + Susp.force sum)
        val (count, xs) = makeTicks 100
        val sum = L.foldl f (Susp.fromVal 0) xs
        val () = Assert.assertEqualInt 0 $ count ()
        val sum = Susp.force sum
      in
        Assert.assertEqualInt 100 $ count ()
      end

  fun foldl'CompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.foldl' op+ 0 xs
      in
        Assert.assertEqualInt 100 $ count ()
      end

  fun foldrCompTest () =
      let
        fun f (n, sum) = Susp.delay (fn () => n + Susp.force sum)
        val (count, xs) = makeTicks 100
        val sum = L.foldr f (Susp.fromVal 0) xs
        val () = Assert.assertEqualInt 0 $ count ()
        val sum = Susp.force sum
      in
        Assert.assertEqualInt 100 $ count ()
      end

  fun foldr'CompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.foldr' op+ 0 xs
      in
        Assert.assertEqualInt 100 $ count ()
      end

  fun hdCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.hd xs
      in
        Assert.assertEqualInt 1 $ count ()
      end

  fun tlCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.tl xs
      in
        Assert.assertEqualInt 1 $ count ()
      end

  fun lengthCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.length xs
      in
        Assert.assertEqualInt 100 $ count ()
      end

  fun lastCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.last xs
      in
        Assert.assertEqualInt 100 $ count ()
      end

  fun getItemCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.getItem xs
      in
        Assert.assertEqualInt 1 $ count ()
      end

  fun nthCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.nth (xs, 0)
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.nth (xs, 10)
        val () = Assert.assertEqualInt 11 $ count ()
        val _ = L.nth (xs, 5)
        val () = Assert.assertEqualInt 11 $ count ()
        val _ = L.nth (xs, 29)
        val () = Assert.assertEqualInt 30 $ count ()
      in
        ()
      end

  fun takeCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.take (xs, 10)
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.take (xs, 0)
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.take (xs, 20)
        val () = Assert.assertEqualInt 0 $ count ()
      in
        ()
      end

  fun dropCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.drop (xs, 10)
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.drop (xs, 0)
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.drop (xs, 20)
        val () = Assert.assertEqualInt 0 $ count ()
      in
        ()
      end

  fun appendCompTest () =
      let
        val (count, xs) = makeTicks 100
        val ys = L.append (xs, L.fromList [1, 2, 3])
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 1 $ count ()
      in
        ()
      end

  fun revAppendCompTest () =
      let
        val (count, xs) = makeTicks 100
        val ys = L.revAppend (xs, L.fromList [1, 2, 3])
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun revCompTest () =
      let
        val (count, xs) = makeTicks 100
        val ys = L.rev xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun concatCompTest () =
      let
        val (count1, xs) = makeTicks 100
        val (count2, ys) = makeTicks 100
        val (count3, zs) = makeTicks 100
        val ws = L.concat $ L.fromList [xs, ys, zs]
        val () = Assert.assertEqualInt 0 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val _ = L.hd ws
        val () = Assert.assertEqualInt 1 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
      in
        ()
      end

  fun appCompTest () =
      let
        val (count, xs) = makeTicks 100
        val () = Assert.assertEqualInt 0 $ count ()
        val () = L.app ignore xs
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun mapCompTest () =
      let
        val (count, xs) = makeTicks 100
        val ys = L.map ignore xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 1 $ count ()
      in
        ()
      end

  fun mapPartialCompTest () =
      let
        fun f n = if n >= 7 andalso even n then SOME (Int.quot (n, 2)) else NONE
        val (count, xs) = makeTicks 100
        val ys = L.mapPartial f xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 9 $ count ()
      in
        ()
      end

  fun findCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.find (fn n => n > 10) xs
        val () = Assert.assertEqualInt 12 $ count ()
        val _ = L.find (fn n => n > 3) xs
        val () = Assert.assertEqualInt 12 $ count ()
      in
        ()
      end

  fun filterCompTest () =
      let
        val (count, xs) = makeTicks 100
        val ys = L.filter even xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.nth (ys, 2)
        val () = Assert.assertEqualInt 5 $ count ()
        val _ = L.nth (ys, 6)
        val () = Assert.assertEqualInt 13 $ count ()
      in
        ()
      end

  fun partitionCompTest () =
      let
        val (count, xs) = makeTicks 100
        val (ys, zs) = L.partition even xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.nth (ys, 2)
        val () = Assert.assertEqualInt 5 $ count ()
        val _ = L.nth (zs, 6)
        val () = Assert.assertEqualInt 14 $ count ()
        val _ = L.nth (ys, 5)
        val () = Assert.assertEqualInt 14 $ count ()
      in
        ()
      end

  fun existsCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.exists (fn n => n > 3) xs
        val () = Assert.assertEqualInt 5 $ count ()
        val _ = L.exists (fn n => n = 2) xs
        val () = Assert.assertEqualInt 5 $ count ()
        val _ = L.exists (fn n => n = 200) xs
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun allCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.all (fn n => n > 3) xs
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.all (fn n => n < 2) xs
        val () = Assert.assertEqualInt 3 $ count ()
        val _ = L.all (fn n => n <> 200) xs
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun intersperseCompTest () =
      let
        val (count, xs) = makeTicks 100
        val ys = L.intersperse (~1, xs)
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.nth (ys, 0)
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.nth (ys, 5)
        val () = Assert.assertEqualInt 3 $ count ()
        val _ = L.nth (ys, 3)
        val () = Assert.assertEqualInt 3 $ count ()
      in
        ()
      end

  fun intercalateCompTest () =
      let
        val (count1, xs) = makeTicks 10
        val (count2, ys) = makeTicks 10
        val (count3, zs) = makeTicks 10
        val (count4, ss) = makeTicks 10
        val ws = L.intercalate (ss, L.fromList [xs, ys, zs])
        val () = Assert.assertEqualInt 0 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val () = Assert.assertEqualInt 0 $ count4 ()
        val _ = L.nth (ws, 0)
        val () = Assert.assertEqualInt 1 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val () = Assert.assertEqualInt 0 $ count4 ()
        val _ = L.nth (ws, 10)
        val () = Assert.assertEqualInt 10 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val () = Assert.assertEqualInt 1 $ count4 ()
        val _ = L.nth (ws, 20)
        val () = Assert.assertEqualInt 10 $ count1 ()
        val () = Assert.assertEqualInt 1 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val () = Assert.assertEqualInt 10 $ count4 ()
        val _ = L.nth (ws, 30)
        val () = Assert.assertEqualInt 10 $ count1 ()
        val () = Assert.assertEqualInt 10 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val () = Assert.assertEqualInt 10 $ count4 ()
        val _ = L.nth (ws, 40)
        val () = Assert.assertEqualInt 10 $ count1 ()
        val () = Assert.assertEqualInt 10 $ count2 ()
        val () = Assert.assertEqualInt 1 $ count3 ()
        val () = Assert.assertEqualInt 10 $ count4 ()
        val _ = L.last ws
        val () = Assert.assertEqualInt 10 $ count1 ()
        val () = Assert.assertEqualInt 10 $ count2 ()
        val () = Assert.assertEqualInt 10 $ count3 ()
        val () = Assert.assertEqualInt 10 $ count4 ()
      in
        ()
      end

  fun cycleCompTest () =
      let
        val (count, xs) = makeTicks 10
        val ys = L.cycle xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.nth (ys, 0)
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.nth (ys, 5)
        val () = Assert.assertEqualInt 6 $ count ()
        val _ = L.nth (ys, 15)
        val () = Assert.assertEqualInt 10 $ count ()
        val _ = L.nth (ys, 7)
        val () = Assert.assertEqualInt 10 $ count ()
      in
        ()
      end

  fun concatMapCompTest () =
      let
        val (count1, xs) = makeTicks 10
        val (count2, ys) = makeTicks 10
        val (count3, zs) = makeTicks 10
        val lls = [xs, ys, zs]
        val ws = L.concatMap (fn n => List.nth (lls, n)) $ L.fromList [0, 1, 2]
        val () = Assert.assertEqualInt 0 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val _ = L.nth (ws, 0)
        val () = Assert.assertEqualInt 1 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val _ = L.nth (ws, 10)
        val () = Assert.assertEqualInt 10 $ count1 ()
        val () = Assert.assertEqualInt 1 $ count2 ()
        val () = Assert.assertEqualInt 0 $ count3 ()
        val _ = L.nth (ws, 20)
        val () = Assert.assertEqualInt 10 $ count1 ()
        val () = Assert.assertEqualInt 10 $ count2 ()
        val () = Assert.assertEqualInt 1 $ count3 ()
        val _ = L.last ws
        val () = Assert.assertEqualInt 10 $ count1 ()
        val () = Assert.assertEqualInt 10 $ count2 ()
        val () = Assert.assertEqualInt 10 $ count3 ()
      in
        ()
      end

  fun splitAtCompTest () =
      let
        val (count, xs) = makeTicks 100
        val (ys, zs) = L.splitAt (xs, 20)
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.nth (ys, 0)
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.nth (ys, 5)
        val () = Assert.assertEqualInt 6 $ count ()
        val _ = L.hd zs
        val () = Assert.assertEqualInt 21 $ count ()
      in
        ()
      end

  fun takeWhileCompTest () =
      let
        val (count, xs) = makeTicks 100
        val ys = L.takeWhile (fn n => n < 30) xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.length ys
        val () = Assert.assertEqualInt 31 $ count ()
      in
        ()
      end

  fun dropWhileCompTest () =
      let
        val (count, xs) = makeTicks 100
        val ys = L.dropWhile (fn n => n < 30) xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 31 $ count ()
        val _ = L.length ys
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun spanCompTest () =
      let
        val (count, xs) = makeTicks 100
        val (ys, zs) = L.span (fn n => n < 30) xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.nth (ys, 15)
        val () = Assert.assertEqualInt 16 $ count ()
        val _ = L.hd zs
        val () = Assert.assertEqualInt 31 $ count ()
        val _ = L.length ys
        val () = Assert.assertEqualInt 31 $ count ()
        val _ = L.nth (zs, 45)
        val () = Assert.assertEqualInt 76 $ count ()
        val _ = L.length zs
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun breakCompTest () =
      let
        val (count, xs) = makeTicks 100
        val (ys, zs) = L.break (fn n => n >= 30) xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.hd ys
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.nth (ys, 15)
        val () = Assert.assertEqualInt 16 $ count ()
        val _ = L.hd zs
        val () = Assert.assertEqualInt 31 $ count ()
        val _ = L.length ys
        val () = Assert.assertEqualInt 31 $ count ()
        val _ = L.nth (zs, 45)
        val () = Assert.assertEqualInt 76 $ count ()
        val _ = L.length zs
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun lengthGreaterThanCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.lengthGreaterThan (xs, 0)
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.lengthGreaterThan (xs, 10)
        val () = Assert.assertEqualInt 11 $ count ()
        val _ = L.lengthGreaterThan (xs, 5)
        val () = Assert.assertEqualInt 11 $ count ()
        val _ = L.lengthGreaterThan (xs, 200)
        val () = Assert.assertEqualInt 100 $ count ()
      in
        ()
      end

  fun zipWithCompTest () =
      let
        val (count1, xs) = makeTicks 100
        val (count2, ys) = makeTicks 200
        val zs = L.zipWith op+ (xs, ys)
        val () = Assert.assertEqualInt 0 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val _ = L.nth (zs, 10)
        val () = Assert.assertEqualInt 11 $ count1 ()
        val () = Assert.assertEqualInt 11 $ count2 ()
        val _ = L.length zs
        val () = Assert.assertEqualInt 100 $ count1 ()
        val () = Assert.assertEqualInt 101 $ count2 ()
      in
        ()
      end

  fun zipCompTest () =
      let
        val (count1, xs) = makeTicks 100
        val (count2, ys) = makeTicks 200
        val zs = L.zip (xs, ys)
        val () = Assert.assertEqualInt 0 $ count1 ()
        val () = Assert.assertEqualInt 0 $ count2 ()
        val _ = L.nth (zs, 10)
        val () = Assert.assertEqualInt 11 $ count1 ()
        val () = Assert.assertEqualInt 11 $ count2 ()
        val _ = L.length zs
        val () = Assert.assertEqualInt 100 $ count1 ()
        val () = Assert.assertEqualInt 101 $ count2 ()
      in
        ()
      end

  fun unzipCompTest () =
      let
        val (count, xs) = makeTicks 100
        val (ys, zs) = L.unzip $ L.map (fn n => (n, n)) xs
        val () = Assert.assertEqualInt 0 $ count ()
        val _ = L.nth (ys, 0)
        val () = Assert.assertEqualInt 1 $ count ()
        val _ = L.nth (zs, 10)
        val () = Assert.assertEqualInt 11 $ count ()
        val _ = L.nth (ys, 5)
        val () = Assert.assertEqualInt 11 $ count ()
      in
        ()
      end

  fun toListCompTest () =
      let
        val (count, xs) = makeTicks 100
        val _ = L.toList xs
      in
        Assert.assertEqualInt 100 $ count ()
      end

  fun suite _ = Test.labelTests [
    ("hdTest", hdTest),
    ("lengthTest", lengthTest),
    ("nullTest", nullTest),
    ("tlTest", tlTest),
    ("nthTest", nthTest),
    ("fromListTest", fromListTest),
    ("toListTest", toListTest),
    ("fromList_toListTest", fromList_toListTest),
    ("lastTest", lastTest),
    ("getItemTest", getItemTest),
    ("collateTest", collateTest),
    ("takeTest", takeTest),
    ("dropTest", dropTest),
    ("appendTest", appendTest),
    ("revAppendTest", revAppendTest),
    ("revTest", revTest),
    ("appTest", appTest),
    ("mapTest", mapTest),
    ("mapPartialTest", mapPartialTest),
    ("findTest", findTest),
    ("filterTest", filterTest),
    ("partitionTest", partitionTest),
    ("existsTest", existsTest),
    ("allTest", allTest),
    ("tabulateTest", tabulateTest),
    ("intersperseTest", intersperseTest),

    ("nullCompTest", nullCompTest),
    ("foldlCompTest", foldlCompTest),
    ("foldl'CompTest", foldl'CompTest),
    ("foldrCompTest", foldrCompTest),
    ("foldr'CompTest", foldr'CompTest),
    ("hdCompTest", hdCompTest),
    ("tlCompTest", tlCompTest),
    ("lengthCompTest", lengthCompTest),
    ("lastCompTest", lastCompTest),
    ("getItemCompTest", getItemCompTest),
    ("nthCompTest", nthCompTest),
    ("takeCompTest", takeCompTest),
    ("dropCompTest", dropCompTest),
    ("appendCompTest", appendCompTest),
    ("revAppendCompTest", revAppendCompTest),
    ("revCompTest", revCompTest),
    ("concatCompTest", concatCompTest),
    ("appCompTest", appCompTest),
    ("mapCompTest", mapCompTest),
    ("mapPartialCompTest", mapPartialCompTest),
    ("findCompTest", findCompTest),
    ("filterCompTest", filterCompTest),
    ("partitionCompTest", partitionCompTest),
    ("existsCompTest", existsCompTest),
    ("allCompTest", allCompTest),
    ("intersperseCompTest", intersperseCompTest),
    ("intercalateCompTest", intercalateCompTest),
    ("cycleCompTest", cycleCompTest),
    ("concatMapCompTest", concatMapCompTest),
    ("splitAtCompTest", splitAtCompTest),
    ("takeWhileCompTest", takeWhileCompTest),
    ("dropWhileCompTest", dropWhileCompTest),
    ("spanCompTest", spanCompTest),
    ("breakCompTest", breakCompTest),
    ("lengthGreaterThanCompTest", lengthGreaterThanCompTest),
    ("zipWithCompTest", zipWithCompTest),
    ("zipCompTest", zipCompTest),
    ("unzipCompTest", unzipCompTest),
    ("toListCompTest", toListCompTest)
  ]
end
