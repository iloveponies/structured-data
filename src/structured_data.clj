(ns structured-data)

(defn do-a-thing [x]
  (let [two-x (+ x x)]
    (Math/pow two-x two-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (Math/abs (- y2 y1))))
  
(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[px py] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and (contains-point? outer inner-bottom-left)
         (contains-point? outer inner-top-right))))


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
  (let [second-elt (fn [x] (get x 1))]
    (map second-elt collection)))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))


; (defn toggle [a-set elem]
;   (if (contains? a-set elem)
;     (disj a-set elem)
;     (conj a-set elem)))

; Instead of the obvious solution using 'if' to select between disj/conj, here's an alternate,
; less readable version ;)
(defn toggle [a-set elem]
  (let [toggle-fn
        {true disj false conj}]
    ((get toggle-fn (contains? a-set elem)) a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)
          (count (set a-seq)))))

; eh...
; (defn contains-duplicates? [a-seq]
;  (not (= a-seq
;          (reverse (apply (partial conj '()) (set a-seq))))))

(defn old-book->new-book [book]
  (assoc book :authors
         (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(comment
(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
                            :birth-year 1947
                            :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer" :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])
)

(defn authors [books]
  (set (apply concat (map :authors books))))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name (:name author)
        lifespan (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (if (contains? author :birth-year)
      (str name lifespan)
      name)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (let [book-count (count books)
        prefix (cond 
                 (== book-count 0) "No books."
                 (== book-count 1) "1 book. "
                 :else (str book-count " books. "))]
    (str prefix (apply str (interpose ". " (map book->string books)))
         (cond (> book-count 0) "."
               :else ""))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [author]
                   (= name (:name author)))
                 authors)))


(defn living-authors [authors]
  (filter (fn [author] (not (contains? author :death-year))) authors))


(defn has-a-living-author? [book]
 (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
