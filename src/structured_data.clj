(ns structured-data)

; (try
;   (def china {:name "China Miéville", :birth-year 1972})
;   (def octavia {:name "Octavia E. Butler"
;                 :birth-year 1947
;                 :death-year 2006})
;   (def friedman {:name "Daniel Friedman" :birth-year 1944})
;   (def felleisen {:name "Matthias Felleisen"})
;
;   (def cities {:title "The City and the City" :authors #{china}})
;   (def wild-seed {:title "Wild Seed", :authors #{octavia}})
;   (def embassytown {:title "Embassytown", :authors #{china}})
;   (def little-schemer {:title "The Little Schemer"
;                        :authors #{friedman, felleisen}})
;   (def books [cities, wild-seed, embassytown, little-schemer])
;   (catch Exception e
;     (prn "caught" e)))

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

;; Write the function (spiff v) that takes a vector and returns
;; the sum of the first and third elements of the vector.
;; What happens when you pass in a vector that is too short?
(defn spiff [v]
  (+ (get v 0)(get v 2)))

;; Write the function (cutify v) that takes a vector as a
;; parameter and adds "<3" to its end.

(defn cutify [v]
  (conj v "<3"))

;; Rewrite our earlier function spiff by destructuring
;; its parameter. Call this new function spiff-destructuring.
(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle)
     (width rectangle)))

(defn area [rectangle]
  (* (height rectangle)
     (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x3 y3] point]
    (and (<= x1 x3 x2)
         (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn[coll] (get coll 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn increasing? [a-seq]
  (apply <= a-seq))

(defn decreasing? [a-seq]
  (apply >= a-seq))

(defn monotonic? [a-seq]
  (or (increasing? a-seq) (decreasing? a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq)(count (set a-seq))))

;; Write the function (old-book->new-book book) that takes a book with
;; the previous representation (authors in a vector) and returns the
;; same book in the new representation (authors in a set).
;; Use assoc to change the representation. Do not construct a
;; new map using the map literal syntax.
(defn old-book->new-book [book]
  (let [authors-set (set (:authors book))]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (contains? author :birth-year)
                 (str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

;; (book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
;; (book->string little-schemer)
;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;
(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

;; (books->string []) ;=> "No books."
;; (books->string [cities])
;; ;=> "1 book. The City and the City, written by China Miéville (1972 - )."
;; (books->string [little-schemer, cities, wild-seed])
;; ;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ),
;; Matthias Felleisen. The City and the City, written by China Miéville
;; (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006).")
(defn books->string [books]
  (let [books-count (count books)
        books-count-str (cond
                         (= books-count 1) "1 book. "
                         (> books-count 1) (str books-count " books. ")
                         :else "No books")
        books-str (apply str (interpose ", " (map book->string books)))]
       (str books-count-str books-str ".")))

(defn books-by-author [author books]
  (let [book-by-author?
        (fn[book] (has-author? book author))]
   (filter book-by-author? books)))

(defn author-by-name [name authors]
  (first
    (filter
      (fn[author] (= name (:name author)))
      authors)))

(defn living-authors [authors]
  (filter alive? authors))

; (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
; (def christopher {:name "Christopher Tolkien" :birth-year 1924})
; (def kay {:name "Guy Gavriel Kay" :birth-year 1954})
;
; (def silmarillion {:title "Silmarillion"
;                    :authors #{jrrtolkien, christopher, kay}})
; (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
; (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})
;
; (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (not (empty?
        (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
