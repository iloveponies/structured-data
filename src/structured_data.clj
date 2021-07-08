(ns structured-data)

(defn do-a-thing [x] (let [self_addition (+ x x)] (Math/pow self_addition self_addition)))

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v] (let [[one two three] v] (+ one three)))

(defn point [x y] [x y])

(defn rectangle [bottom-left top-right] [bottom-left top-right])

(defn width [rectangle] (let [[[x1 y1] [x2 y2]] rectangle] (Math/abs (- x1 x2))))

(defn height [rectangle] (let [[[x1 y1] [x2 y2]] rectangle] (Math/abs (- y1 y2))))

(defn square? [rectangle] (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle] (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point] (let [[[x1 y1] [x2 y2]] rectangle [px py] point]
  (if (and (<= x1 px x2) (<= y1 py y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[inner_bottom_left inner_up_right] inner]
    (if (and (contains-point? outer inner_bottom_left) (contains-point? outer inner_up_right))
      true false)))

(defn title-length [book] (count (:title book)))

(defn author-count [book] (count (:authors book)))

(defn multiple-authors? [book] (if (< 1 (count (:authors book))) true false))

(defn add-author [book new-author] (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author] (if (= nil (:death-year author)) true false))

(defn element-lengths [collection] (map count collection))

(defn second-elements [collection] (let [get_second (fn [v] (get v 1))] (map get_second collection)))

(defn titles [books] (map :title books))

(defn monotonic? [a-seq] (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq] (if (< (count (set a-seq)) (count a-seq)) true false))

(defn old-book->new-book [book] (assoc book :authors (set (:authors book))))

(defn has-author? [book author] (contains? (:authors book) author))

(defn authors [books] (apply clojure.set/union (map :authors books)))

(defn all-author-names [books] (set (map :name (authors books))))

(defn author->string [author]
  (let [get_life_info (fn [author]
                        (if (not (= nil (:birth-year author)))
                          (if (= nil (:death-year author))
                            (str " (" (:birth-year author) " - )")
                            (str " (" (:birth-year author) " - " (:death-year author) ")"))
                          ""))]
  (str (:name author) (get_life_info author))
  ))

(defn authors->string [authors] (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [
        author_presentation
        (fn [book]
          (if (= nil (:authors book))
            ""
            (str ", written by " (authors->string (:authors book))))
          )]
  (str (:title book) (author_presentation book))))

(defn books->string [books]
  (let [how_many_books (fn [books] (if (== 1 (count books)) "1 book. " (if (== 0 (count books)) "No books" (str (count books) " books. "))))]
  (str (how_many_books books) (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books] (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors] (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors] (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book] (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books] (filter (fn [book] (has-a-living-author? book)) books))

; %________%
