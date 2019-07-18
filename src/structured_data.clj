(ns structured-data)

(defn do-a-thing [x]
  (let [dx (+ x x)]
        (Math/pow dx dx)))

(defn spiff [v]
  (defn get-i [i]
    (or (get v i) 0))
  (reduce + (map get-i [0 2])))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
   (+ (or a 0) (or c 0))))

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
    (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle]
      (let [[x3 y3] point]
        (and (<= x1 x3 x2) (<= y1 y3 y2)))))


(defn contains-rectangle? [outer inner]
    (let [[bottom-left top-right] inner]
      (and (contains-point? outer bottom-left)
           (contains-point? outer top-right))))

(defn count-value [key book]
  (count (key book)))

(defn title-length [book]
  (count-value :title book))

(defn author-count [book]
  (count-value :authors book))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [inner] (get inner 1))]
        (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn my-repeat [n txt]
  (loop [cnt n s txt] 
    (cond (<= cnt 0) ""
          (= cnt 1) s
          :else (recur (dec cnt) (str s s)))))

(defn toggle [a-set elem]
  (cond (contains? a-set elem) (disj a-set elem)
        :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn year-range [author]
  (cond
   (:birth-year author) (format "(%s - %s)" (or (:birth-year author) "") (or (:death-year author) ""))
   :else ""))

(defn author->string [author]
  (clojure.string/trim (clojure.string/join " " [(:name author) (year-range author)])))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (interpose ", written by " [(:title book) (authors->string (:authors book))])))

(defn books->string [books]
  (let [cnt (count books)]
    (cond
     (== cnt 0) "No books."
     (== cnt 1) (format "1 book. %s." (book->string (first books)))
     :else (format "%d books. %s." cnt (apply str (interpose ". " (map book->string books)))))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter 
          (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (not (contains? author :death-year))) authors))

(defn has-a-living-author? [book]
  (-> (living-authors (:authors book))
      first
      nil?
      not))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
