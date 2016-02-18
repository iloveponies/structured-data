(ns structured-data)

(defn do-a-thing [x]
  (let [dub-x (+ x x)]
    (Math/pow dub-x dub-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ c]]
  (+ a c)) 

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn abs [x]
  (if (> x 0)
    x
    (* -1 x)))

(defn width [[[x1 _] [x2 _]]]
  (abs (- x1 x2)) )

(defn height [[[_ y1] [_ y2]]]
  (abs (- y1 y2)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [xp yp]]
  (or
   (and (>= x1 xp x2) (>= y1 yp y2))
   (and (>= x2 xp x1) (>= y2 yp y1))
  ))

(defn contains-rectangle? [outer [inner-p1 inner-p2]]
  (and (contains-point? outer inner-p1) (contains-point? outer inner-p2)) )

(defn test-contains-pt [] (contains-point? (rectangle [0 0] [2 2])
                                           (point 1 1)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [coll] (get coll 1)) collection))

(defn titles [books]
  (map (fn [data] (:title data)) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
   (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [b] (:authors b)) books ) ))

(defn all-author-names [books]
  (set  (map (fn [b] (:name b)) (authors books))))

(defn author->string [author]
  (let [name  (str (:name author))
        years (cond
                 (contains? author :death-year)
                 (str " (" (:birth-year author) " - " (:death-year author) ")")
                 (contains? author :birth-year)
                 (str " (" (:birth-year author) " - " ")")
                 :else "")]
       (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (= 0 (count books)) "No books."
      (let [num-books (cond 
                            (= 1 (count books)) "1 book."
                            :else (str (count books) " books."))
            booklist (apply str (interpose ". " (map book->string books)) )]
        (str num-books " " booklist "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
 (first (filter (fn [auth] (= name (:name auth))) authors)) )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})


; %________%
