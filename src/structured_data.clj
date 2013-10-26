(ns structured-data)

(defn do-a-thing [x]
  (let [MyFoo (+ x x)]
   (Math/pow MyFoo MyFoo)))

(defn spiff [v]
  (let [eka (get v 0)
        golmazEiVarmaanOleVarattuSana (get v 2)]
   (+ eka golmazEiVarmaanOleVarattuSana)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
   (+ a c)))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
   (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner_bottom_left inner_top_right] inner]
   (and (contains-point? outer inner_bottom_left)
        (contains-point? outer inner_top_right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old_authors (:authors book)
        new_authors (conj old_authors new-author)]
    (assoc book :authors new_authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second_item (fn [vctor] (get vctor 1))]
    (map second_item collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (:name author)
        kualinvuosi (:death-year author)
        synnyinvuasi (:birth-year author)]
     (if(contains? author :birth-year)
       (str nimi " (" synnyinvuasi " - " kualinvuosi ")")
       (str nimi))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [amount (count books)
        selectBook (fn[book](book->string book))]
   (cond(= 0 amount) "No books."
    (= 1 amount) (str amount " book. " (apply book->string books)".")
    :else (str amount " books. " (apply str (interpose ". "(map selectBook books)))"."))))

(defn books-by-author [author books]
  (filter (fn [books] (has-author? books author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%





