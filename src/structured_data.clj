(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


(defn spiff [v]
  (let [eka (get v 0)
        toka (get v 2)]
    (+ eka toka)))


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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))


(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- y2 y1) (- x2 x1))
      true
      false)))


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
      (if (and (>= x x1) (<= x x2) (>= y y1) (<= y y2))
        true
        false))))


(defn contains-rectangle? [outer inner]
  (let [[[x1_out y1_out] [x2_out y2_out]] outer]
    (let [[[x1_in y1_in] [x2_in y2_in]] inner]
      (if (and (>= x1_in x1_out) (<= x2_in x2_out) (>= y1_in y1_out) (<= y2_in y2_out))
        true
        false))))


(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (if (>= (count (:authors book)) 2 )
    true
    false))


(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))


(defn alive? [author]
  (if (not (contains? author :death-year))
    true
    false))


; alla olevat kaksi funktiota kuuluvat samaan toteutukseen
; funktiota pituus kaytetaan jalkimmaisessa

(defn pituus [x]
  (count x))

(defn element-lengths [collection]
  (map pituus collection))


(defn second-elements [collection]
  (let [toinenAlkio (fn [x] (get x 1))]
    (map toinenAlkio collection)))


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (let [x (set a-seq)]
    (if (= (count a-seq) (count x))
      false
      true)))


(defn old-book->new-book [book]
  (let [authorset (set (:authors book))]
    (assoc book :authors authorset)))


(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))


(defn authors [books]
  (let [author-names
         (fn [book] (:authors book))]
    (set (apply clojure.set/union (map author-names books)))))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if (contains? author :birth-year)
      (str name " (" birth " - " death ")")
      (str name))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (if (contains? book :authors)
      (str title ", written by " (authors->string authors))
      (str title))))


(defn books->string [books]
  (let [amount (count books)]
    (cond
     (= amount 0) "No books."
     (= amount 1) (apply str(concat "1 book. " (interpose "." (map book->string books)) "."))
     :else (apply str amount (concat " books. " (interpose ". " (map book->string books)) ".")))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (if (contains? (set (map :name authors)) name)
    (first (filter (fn [author] (= name (:name author))) authors))
    nil))


(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))


(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (if (contains? (set (map :death-year authors)) nil)
      true
      false)))


(defn books-by-living-authors [books]
  (let [living (fn [book] (has-a-living-author? book))]
    (filter living books)))


; %_______%
