(ns my-tests
  (:use
    midje.sweet
    structured-data))


  (fact "do-a-thing"
    (do-a-thing 1) => 4.0
  )

  (fact "spiff"
    (spiff [1 2 3])       => 4
    (spiff [1 2 3 4 5 6]) => 4
    (spiff [1 2])         => nil
    (spiff [])            => nil
  )

  (fact "cutify"
    (cutify []) => ["<3"]
    (cutify [1 2 3]) => [1 2 3 "<3"]
    (cutify ["a" "b"]) => ["a" "b" "<3"]
  )

  (fact "spiff-destructuring"
    (spiff-destructuring [1 2 3])       => 4
    (spiff-destructuring [1 2 3 4 5 6]) => 4
    (spiff-destructuring [1 2])         => nil
    (spiff-destructuring [])            => nil
  )

  (fact "height"
    (height (rectangle [1 1] [5 1])) => 0
    (height (rectangle [1 1] [5 5])) => 4
    (height (rectangle [0 0] [2 3])) => 3
  )

  (fact "width"
    (width (rectangle [1 1] [5 1]))  => 4
    (width (rectangle [1 1] [1 1]))  => 0
    (width (rectangle [3 1] [10 4])) => 7
  )

  (fact "square?"
    (square? (rectangle [1 1] [2 2])) => true
    (square? (rectangle [1 1] [2 3])) => false
    (square? (rectangle [1 1] [1 1])) => true
    (square? (rectangle [3 2] [1 0])) => true
    (square? (rectangle [3 2] [1 1])) => false
  )

  (fact "area"
    (area (rectangle [1 1] [5 1]))  => 0
    (area (rectangle [0 0] [1 1]))  => 1
    (area (rectangle [0 0] [4 3]))  => 12
    (area (rectangle [3 1] [10 4])) => 21
  )

  (fact "contains-point?"
    (contains-point? (rectangle [0 0] [2 2])
                    (point 1 1))             => true
    (contains-point? (rectangle [0 0] [2 2])
                    (point 2 1))             => true
    (contains-point? (rectangle [0 0] [2 2])
                    (point -3 1))            => false
    (contains-point? (rectangle [0 0] [2 2])
                    (point 1 3))             => false
    (contains-point? (rectangle [1 1] [2 2])
                    (point 1 1))             => true
    (contains-point? (rectangle [1 1] [1 1])
                    (point 1 1))             => true
  )

  (fact "contains-rectangle?"
    (contains-rectangle? (rectangle [0 0] [3 3])
                        (rectangle [1 1] [2 2])) => true
    (contains-rectangle? (rectangle [0 0] [2 2])
                        (rectangle [1 1] [3 3])) => false
    (contains-rectangle? (rectangle [0 0] [1 1])
                        (rectangle [0 0] [1 1])) => true
    (contains-rectangle? (rectangle [0 0] [1 1])
                        (rectangle [1 1] [2 2])) => false
  )

  (fact "contains-rectangle?"
    (title-length cities)         => 21
    (title-length wild-seed)      => 9
    (title-length little-schemer) => 18
  )

  (fact "author-count"
    (author-count cities)         => 1
    (author-count wild-seed)      => 1
    (author-count little-schemer) => 2
  )

  (fact "multiple-authors?"
    (multiple-authors? cities)         => false
    (multiple-authors? wild-seed)      => false
    (multiple-authors? little-schemer) => true
  )

  (fact "add-author"
    (author-count (add-author cities {:name "foo"})) => 2
  )

  (fact "alive?"
    (alive? china)   => true
    (alive? octavia) => false
  )

  (fact "element-lengths"
    ;; (print (element-lengths ["foo" "bar" "" "quux"]))
    (element-lengths ["foo" "bar" "" "quux"])  => [ 3 3 0 4 ]
    (element-lengths ["x" [:a :b :c] {:y 42}]) => [ 1 3 1 ]
  )

  (fact "element-lengths"
    (second-elements [[1 2] [2 3] [3 4]]) => [ 2 3 4 ]
    (second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]]) => [ 2 nil "s" ]
  )

  (fact "element-lengths"
    ;; (def china {:name "China Miéville", :birth-year 1972})
    ;; (def octavia {:name "Octavia E. Butler"
    ;;               :birth-year 1947
    ;;               :death-year 2006})
    ;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
    ;; (def felleisen {:name "Matthias Felleisen"})
    ;;
    ;; (def cities {:title "The City and the City" :authors [china]})
    ;; (def wild-seed {:title "Wild Seed", :authors [octavia]})
    ;; (def embassytown {:title "Embassytown", :authors [china]})
    ;; (def little-schemer {:title "The Little Schemer"
    ;;                     :authors [friedman, felleisen]})

    (def books [cities, wild-seed, embassytown, little-schemer])

    (titles [cities]) => [ "The City and the City" ]
    (titles books) => [ "The City and the City" "Wild Seed" "Embassytown" "The Little Schemer" ]
  )

  (fact "stars"
    (stars 1) => "*"
    (stars 7) => "*******"
    (stars 3) => "***"
  )

  (fact "monotonic?"
    (monotonic? [1 2 3])     => true
    (monotonic? [0 1 10 11]) => true
    (monotonic? [3 2 0 -3])  => true
    (monotonic? [3 2 2])     => true    ;Not strictly monotonic
    (monotonic? [1 2 1 0])   => false
  )

  (fact "toggle"
    (toggle #{:a :b :c} :d) => #{:a :c :b :d}
    (toggle #{:a :b :c} :a) => #{:c :b}
  )

  (fact "contains-duplicates?"
    (contains-duplicates? [1 1 2 3 -40]) => true
    (contains-duplicates? [1 2 3 -40]) => false
    (contains-duplicates? [1 2 3 "a" "a"]) => true
  )

  (fact "old-book->new-book"
    (old-book->new-book {:title "The Little Schemer"
                        :authors [friedman, felleisen]})
      => {:title "The Little Schemer" :authors #{friedman, felleisen}}

    (old-book->new-book {:title "Wild Seed", :authors [octavia]})
      => {:title "Wild Seed", :authors #{octavia}}

    (old-book->new-book
      {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
                "British Science Fiction Award"]
      :title "The City and the City"
      :authors [{:birth-year 1972, :name "China Miéville"}]})
      => {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
                  "British Science Fiction Award"]
          :title "The City and the City"
          :authors #{{:birth-year 1972, :name "China Miéville"}}}
  )

  (fact "has-author?"
    (has-author? cities china)             => true
    (has-author? cities felleisen)         => false
    (has-author? little-schemer felleisen) => true
    (has-author? little-schemer friedman)  => true
    (has-author? little-schemer octavia)   => false
  )

  (fact "authors"
    (authors [cities, wild-seed])              ;=> #{china, octavia}
    (authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
    (authors [little-schemer, cities])         => #{china, friedman, felleisen}
  )

  (fact "all-author-names"
    (all-author-names books)
      => #{"Matthias Felleisen" "China Miéville" "Octavia E. Butler" "Daniel Friedman"}

    (all-author-names [cities, wild-seed]) => #{"China Miéville" "Octavia E. Butler"}
    (all-author-names []) => #{}
  )

  (fact "author->string"
    (author->string felleisen) => "Matthias Felleisen"
    (author->string friedman)  => "Daniel Friedman (1944 - )"
    (author->string octavia)   => "Octavia E. Butler (1947 - 2006)"
  )

  (fact "authors->string"
    (authors->string (:authors little-schemer))
      => "Daniel Friedman (1944 - ), Matthias Felleisen"
    (authors->string #{octavia})=> "Octavia E. Butler (1947 - 2006)"
    (authors->string #{}) => ""
    (authors->string #{octavia, friedman})
      => "Daniel Friedman (1944 - ), Octavia E. Butler (1947 - 2006)"
      ;; => (or
      ;;      "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
      ;   order doesn't matter
  )

  (fact "book->string"
    (book->string wild-seed) => "Wild Seed, written by Octavia E. Butler (1947 - 2006)"
    (book->string little-schemer)
      => "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
  )

  (fact "books->string"
    (books->string []) => "No books."
    (books->string [cities])
      => "1 book. The City and the City, written by China Miéville (1972 - )."
    (books->string [little-schemer, cities, wild-seed])
      => (str
           "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), "
          "Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). "
          "Wild Seed, written by Octavia E. Butler (1947 - 2006).")
  )

  (fact "books-by-author"
    (books-by-author china books)   => [ cities embassytown ]
    (books-by-author octavia books) => [ wild-seed ]
  )


  (fact "author-by-name"
    (let [ authors #{china, felleisen, octavia, friedman}]
      (author-by-name "Octavia E. Butler" authors)                => octavia
      (author-by-name "Octavia E. Butler" #{felleisen, friedman}) => nil
      (author-by-name "China Miéville" authors)                   => china
      (author-by-name "Goerge R. R. Martin" authors)              => nil
      )
  )

  (fact "living-authors"
    (let [ authors #{china, felleisen, octavia, friedman}]
      (living-authors authors)             => (contains [china felleisen friedman] :in-any-order)
      (living-authors #{octavia})          => []
      (living-authors #{china, felleisen}) => (contains [china, felleisen] :in-any-order)
    )
  )

  (fact "has-a-living-author?"
    (has-a-living-author? wild-seed)      => false
    (has-a-living-author? silmarillion)   => true
    (has-a-living-author? little-schemer) => true
    (has-a-living-author? cities)         => true
    (has-a-living-author? deus-irae)      => false
  )

  (fact "books-by-living-authors"
    ;; (let [new-books (map old-book->new-book books)]
    ;; (books-by-living-authors books) => [ little-schemer cities embassytown ]
    (books-by-living-authors books) => (contains [little-schemer cities embassytown] :in-any-order)
    (books-by-living-authors (concat books [deus-irae, silmarillion]))
      => (contains [ little-schemer cities embassytown silmarillion ] :in-any-order)
  )


