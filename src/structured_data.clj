(ns structured-data)

(defn do-a-thing [x]
  (let [two-x (+ x x)]
    (Math/pow two-x two-x)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v
        "<3"))

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[bot-left-x bot-left-y] [top-right-x top-right-y]]]
  (- top-right-x bot-left-x))

(defn height [[[bot-left-x bot-left-y] [top-right-x top-right-y]]]
  (- top-right-y
     bot-left-y))

(defn square? [rectangle]
  (= (width rectangle)
     (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [[[bot-left-x bot-left-y] [top-right-x top-right-y]]
                       [point-x point-y]]
  (and (<= bot-left-x
           point-x
           top-right-x)
       (<= bot-left-y
           point-y
           top-right-y)))

(defn contains-rectangle? [outer [inner-bottom-left inner-top-right]]
  (and (contains-point? outer inner-bottom-left)
       (contains-point? outer inner-top-right)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book)
     1))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book
      :authors
      (conj authors new-author))))

(defn alive? [author]
  (not (contains? author
                  :death-year)))

(defn element-lengths [collection]
  (map count
       collection))

(defn second-elements [collection]
  (map (fn [vector]
         (get vector 1))
       collection))

(defn titles [books]
  (map :title
       books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str
         (repeat n
                 "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)
          (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book
    :authors
    (set (get book
              :authors))))

(defn has-author? [book author]
  (contains? (get book :authors)
             author))

(defn authors [books]
  (apply clojure.set/union (map :authors
                                books)))

(defn all-author-names [books]
  (set (map :name
            (authors books))))

(defn author->string [author]
  (str (get author :name)
       (when (contains? author :birth-year)
         (str
          " ("
          (get author :birth-year)
          " - "
          (get author :death-year)
          ")"))))

(defn authors->string [authors]
  (apply str (interpose ", "
                        (map author->string
                             authors))))

(defn book->string [book]
  (str (get book :title)
       ", written by "
       (authors->string (get book :authors))))

(defn books->string [books]
  (let [book-count (count books)]
    (if (> book-count
           0)
      (str book-count
           " book"
           (when (> book-count
                    1)
             "s")
           ". "
           (apply str (interpose ". "
                                 (map book->string
                                      books)))
           ".")
      "No books.")))

(defn books-by-author [author books]
  (filter (fn [book]
            (has-author? book author))
          books))

(defn author-by-name [name authors]
  (first (filter (fn [author]
                   (= (get author :name)
                      name))
                 authors)))

(defn living-authors [authors]
  (filter alive?
          authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
