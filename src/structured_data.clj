(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
  (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (- (second (second rectangle)) (second (first rectangle))))

(defn width [rectangle]
  (- (first (second rectangle)) (first (first rectangle))))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle))
  true
  false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (if (and (<= (first (first rectangle)) (first point) (first (second rectangle)))
           (<= (second (first rectangle)) (second point) (second (second rectangle))))
    true
    false))

(defn contains-rectangle? [outer inner]
  (if (and (>= (first (first inner)) (first (first outer)))
           (>= (second (first inner)) (second (first outer)))
           (<= (first (second inner)) (first (second outer)))
           (<= (second (second inner)) (second (second outer))))
    true
    false))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (== (author-count book) 1)
    false
    true))

(defn add-author [book new-author]
  :-)

(defn alive? [author]
  (if (:death-year author)
  false
  true))

(defn element-lengths [collection]
  (let [laske (fn [x] (count x))]
   (map laske collection)))

(defn second-elements [collection]
  (let [seconds (fn [x] (first (rest x)))]
    (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (let [rivi (repeat n "*")]
    (apply str rivi)))

(defn toggle [a-set elem]
 (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)))

  (defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq))
    false
    true))

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  (let [name (:name author)]
    (if (:birth-year author)
    (let [birth-year (:birth-year author)]
      (let [death-year (:death-year author)]
        (str name " (" birth-year " - " death-year ")")))
    (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)]
    (str title ", written by "(authors->string (:authors book)))))

(defn books->string [books]
  (let [numbooks (count books)]
    (let [authors (apply str (interpose ". " (map book->string books)))]
    (if (= numbooks 0)
      "No books."
      (if (> numbooks 1)
        (str numbooks " books. " authors ".")
        (str numbooks " book. " authors "."))))))

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
    (if (empty? (filter alive? (:authors book)))
      false
      true))

(defn books-by-living-authors [books]
  (map :title (filter has-a-living-author? books)))

; %________%
