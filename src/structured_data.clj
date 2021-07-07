(ns structured-data)

(defn do-a-thing [x]
  (let [ value (+ x x)]
    (Math/pow value value)))

(defn spiff
  [v]
  "[0]+[3]"
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))


(def liste-spiff [[1 2 3] (vec (range 1 7))])

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x a y] v]
    (+ x y)))


(defn point [x y]
  [x y])


(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[bottom-left top-right ] rectangle[x-left] bottom-left [x-right] top-right]
    (Math/abs (-  x-right x-left))))

(defn swap
  [[x y]]
  [y x])

(defn sym-rect
  [rectangle]
  (map swap rectangle))

(defn height [rectangle]
  (Math/abs (width (sym-rect rectangle))))



(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (* w h)))


(defn contains-point? [rectangle point]
  (let [ [x y] point
        [[xmin ymin] [xmax ymax ]] rectangle]
    (if (and (and (<= xmin x ) (>= xmax x))
             (and (<= ymin y ) (>= ymax y)))
      true
      false)))


(defn contains-rectangle? [outer inner]
  (let [[point-left point-right] inner]
    (and (contains-point? outer point-left)
         (contains-point? outer point-right))))


 (defn title-length [book]
   "return length :title"
   (count (:title book)))


(defn author-count [book]
  "return number authors"
  (count (:authors book)))

(defn multiple-authors? [book]
  ((complement #(= 1 %)) (author-count book)))

(defn add-author [book new-author](assoc book :authors (conj (:authors book) new-author)))



(defn alive? [author]
  ((complement contains?) author :death-year))

;; (alive? china)   ;=> true
;; (alive? octavia) ;=> false


(defn element-lengths [collection]
  "return [count(item)]"
 (map count collection))


"(element-lengths ["1" [1 2 3] "1223"])"


(defn second-elements [collection]
  (map #(get % 1) collection))

"(second-elements [[1 2] [2 3]])"


(defn titles [books]
 (map :title books))
"(titles [cities wild-seed])"


(defn monotonic? [a-seq]
 (or  (apply <=  a-seq) (apply >= a-seq)))

;;  (monotonic? [1 2 3])     ;=> true
;; (monotonic? [0 1 10 11]) ;=> true
;; (monotonic? [3 2 0 -3])  ;=> true
;; (monotonic? [3 2 2])     ;=> true    Not strictly monotonic
;; (monotonic? [1 2 1 0])   ;=> false


(defn stars [n]
  (apply str (repeat n "*")))

;; (stars 10)


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

;; (toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
;; (toggle #{:a :b :c} :a) ;=> #{:c :b}

(defn contains-duplicates? [a-seq]
(let [ size-liste (count a-seq)
       size-set (count (set a-seq))]
  ((complement =) size-liste size-set)))


;; "(contains-duplicates? [1 1 1 3 -40])


(count (set [ 1 1 1 1]))

(defn old-book->new-book [book]
  (let [ authors-list (:authors book)]
    (assoc book :authors (set authors-list))))

;; "(old-book->new-book
;;   {:awards ["Hugo" "World Fantasy Award" "Arthur C. Clarke Award"
;;             "British Science Fiction Award"]
;;    :title "The City and the City"
;;    :authors [{:birth-year 1972, :name "China Miéville"}]})"


(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

;; (has-author? cities china)             ;=> true
;; (has-author? cities felleisen)         ;=> false
;; (has-author? little-schemer felleisen) ;=> true
;; (has-author? little-schemer friedman)  ;=> true
;;(has-author? little-schemer octavia)    ;=>false

(defn authors [books]
  (apply clojure.set/union (map :authors (map #(old-book->new-book %) books))))

;; (authors [cities, wild-seed])              ;=> #{china, octavia}
;; (authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}


(defn all-author-names [books]
   (set (map :name (authors books))))

;; (all-author-names [cities, wild-seed])
;; (all-author-names [cities, wild-seed])
;; (all-author-names []) ;=> #{}


(defn author->string [author]
  (let[name (:name author)
       birth (:birth-year author)
       death (:death-year author)]
    (str (str name) (if (contains? author :birth-year)(str " ("birth " - " death ")")))
    )
  )


;; (author->string felleisen) ;=> "Matthias Felleisen"
;; (author->string friedman)  ;=> "Daniel Friedman (1944 - )"
;;(author->string octavia)   ;=> "Octavia E. Butler (1947 - 2006)"

(defn authors->string [authors]
   (apply str (interpose ", " (map #(author->string %)  authors))))

;;  (authors->string (:authors little-schemer))
;; (authors->string #{octavia, friedman})
;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
;   order doesn't matter

(defn book->string [book]
  (apply str (interpose ", written by " [(:title book) (authors->string (:authors book))])))

;; (book->string wild-seed)
;; "Wild Seed, written by Octavia E. Butler"
;;(book->string little-schemer)
;; "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"


(defn books->string [books]
  (let [number-books (count books)]
  (if (empty? books) "No books."
    (do
    (apply str (str number-books " book" (if (< 1 number-books) "s") ".")
            (map #(str " " % ".")
           (map #(book->string %) books)))))))

;; (books->string [cities])
;; ;=> "1 book. The City and the City, written by China Miéville (1972 - ).
;; (books->string [])
;; (books->string [little-schemer, cities, wild-seed])
;; ; "3 books. The Little Schemer, written by Daniel Friedman (1944 - ),
;; ;Matthias Felleisen. The City and the City, written by China Miéville (1972 - ).
;; ;Wild Seed, written by Octavia E. Butler (1947 - 2006)."

(defn books-by-author [author books]
 (filter #(has-author? % author) books))

;;(books-by-author china books)  ;=> (cities embassytown)

(defn author-by-name [name authors]
 (let [ list-by-name (filter #(= name (:name %)) authors )]
    (if (empty? list-by-name)
      nil
       (into {} list-by-name))))

;; (author-by-name "Octavia E. Butler" (authors books))    ;=> octavia
;; (author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
;; (author-by-name "China Miéville" (authors books))        ;=> china
;; (author-by-name "Goerge R. R. Martin" (authors books))


(defn living-authors [authors]
  (filter #(alive? %) authors))


(defn has-a-living-author? [book]
 ((complement empty?) (living-authors (:authors book))))

;; (has-a-living-author? wild-seed)      ;=> false
;; (has-a-living-author? silmarillion)   ;=> true
;; (has-a-living-author? little-schemer) ;=> true
;; (has-a-living-author? cities)         ;=> true
;; (has-a-living-author? deus-irae)      ;=> false

(defn books-by-living-authors [books]
  (filter #(has-a-living-author?  %) books))

;; (count (books-by-living-authors books))
;; ;=> (little-schemer cities embassytown)
;; (count (books-by-living-authors (concat books [deus-irae, silmarillion])))
;; ;=> (little-schemer cities embassytown silmarillion)

;; (def books [cities, wild-seed, embassytown, little-schemer])

;; (def authors #{china, felleisen, octavia, friedman})


;; (def liste-square [(rectangle [1 1][2 2])
;;                    (rectangle [1 1][2 3])
;;                    (rectangle [1 1][1 1])
;;                    (rectangle [3 2][1 0])])



;; (def china {:name "China Miéville"  :birth-year 1972})
;; (def octavia {:name "Octavia E. Butler"
;;               :birth-year 1947
;;               :death-year 2006})
;; (def friedman {:name "Daniel Friedman" :birth-year 1944})
;; (def felleisen {:name "Matthias Felleisen"})

;; (def cities {:title "The City and the City" :authors [china]})
;; (def wild-seed {:title "Wild Seed", :authors [octavia]})
;; (def embassytown {:title "Embassytown", :authors [china]})
;; (def little-schemer {:title "The Little Schemer"
;;                      :authors [friedman, felleisen]})

;; (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;; (def christopher {:name "Christopher Tolkien" :birth-year 1924})
;; (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;; (def silmarillion {:title "Silmarillion"
;;                    :authors #{jrrtolkien, christopher, kay}})
;; (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;; (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;; (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})
