; Sean R. Lang
; (014749564)

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

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (- x2 x1)))
        

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[x y] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [x] (first (rest x)))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn stars [n]
  (apply str (repeat n \*)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    ; disjoin because we have the element
    (disj a-set elem)
    ; join because we don't have the element
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  ; shamelessly taken from the course materials
  ; hey, if you're gonna give me the resource, I'm gonna use it
  ;(let [author-names
  ;      (fn [book] (map :name (:authors book)))]
  ;  (set (apply concat (map author-names books)))))
  ; nevermind, I guess we're not allowed to
  (set (map :name (authors books))))

(defn author->string [author]
  ; "name" was reserved according to vim highlighting, and since one name
  ; is in finnish, the other might as well be too
  (let [nimi (:name author)
        vuotta (if (contains? author :birth-year)
                 (apply str (interpose "" ["("
                                            (:birth-year author)
                                            " - "
                                            (:death-year author)
                                            ")"]))
                 nil)]
    (if vuotta
      ; need the if clause to avoid trailing space
      (apply str (interpose " " [nimi vuotta]))
      nimi)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (interpose "" [(:title book) 
                            ", written by "
                            (authors->string (:authors book))] )))

(defn books->string [books]
  (let [c (count books)
        num-str (cond
                  (== 0 c)  "No books."
                  (== 1 c)  "1 book."
                  :else     (apply str (interpose " " [c "books."])))
        books-str (apply str (interpose ". " (map book->string books)))]
    (if (== 0 c)
      num-str
      (apply str (interpose "" [num-str " " books-str "."])))))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  ;(filter (fn [a] (alive? a)) )
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
