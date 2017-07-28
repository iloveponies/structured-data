(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)
    ))

(defn spiff [v]
  (let [st (get v 0)
       rd (get v 2)]
      (+ st rd)
    ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[st nd rd] v]
      (+ st rd)
    ))

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
  (let [h (height rectangle)
       w (width rectangle)]
      (= h w)
    ))

(defn area [rectangle]
  (let [h (height rectangle)
       w (width rectangle)]
      (* h w)
    ))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
       [[x3 y3] [x4 y4]] inner]
    (and (<= x1 x3 x4 x2) (<= y1 y3 y4 y2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [bookauthors (:authors book)]
    (assoc book :authors (conj bookauthors new-author))
    ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [seconds (fn [e] (get e 1))]
    (map seconds collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
     :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [old (count a-seq)
       new (count (set a-seq))]
  (not (= old new))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
  (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [authorname (:name author)
        birthyear (:birth-year author)
        deathyear (:death-year author)]
    (cond
      (contains? author :death-year) (apply str [authorname " (" birthyear " - " deathyear ")"])
      (contains? author :birth-year) (apply str [authorname " (" birthyear " - )"])
      :else (apply str [authorname])
      )
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [bookname (:title book)
        authorstring (:authors book)]
    (apply str [bookname ", written by " (authors->string authorstring)])
    ))

(defn books->string [books]
  (let [bookcount (count books)
        bookcountstring (apply str [bookcount " count."])
        booksstring (apply str (interpose ". " (map book->string books)))]
    (cond
      (= bookcount 0) "No books."
      (= bookcount 1) (apply str ["1 book. " booksstring "."])
      :else (apply str [bookcount " books. " booksstring "."])
    )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
