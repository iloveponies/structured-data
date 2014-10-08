(ns structured-data)

(defn do-a-thing [x] (let[addx (+ x x)]
                         (Math/pow addx addx) ))

(defn spiff [v] (+ (or (get v 0) 0) (or (get v 2) 0)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v] (let [[a _ b] v]
                                 (+ a b)))

(defn point [x y] [x y])

(defn rectangle [bottom-left top-right] [bottom-left top-right])

(defn width [rectangle] (let[[[x y] [x1 y1]] rectangle] (- x1 x)))

(defn height [rectangle] (let[[[x y] [x1 y1]] rectangle] (- y1 y)))

(defn square? [rectangle] (if(= (width rectangle) (height rectangle))true false))

(defn area [rectangle] (* (width rectangle) (height rectangle )))

(defn contains-point? [rectangle point] (let[[[x y][x1 y1]]rectangle
                                             [a b] point]
                                           (if (and (<= x a x1) (<= y b y1)) true false)))

(defn contains-rectangle? [outer inner] (if(and (contains-point? outer (get inner 0) ) (contains-point? outer (get inner 1))) true false))

(defn title-length [book] (count (:title book)))

(defn author-count [book] (count (:authors book)))

(defn multiple-authors? [book] (> (count (:authors book)) 1))

(defn add-author [book new-author] (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author] (not (contains? author :death-year)))

(defn element-lengths [collection] (map count collection))

(defn second-elements [collection] (let [second (fn [x] (get x 1))]
                                      (map second collection)))

(defn titles [books] (map :title books ))

(defn monotonic? [a-seq] (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq] (> (count a-seq) (count (set a-seq ))))

(defn old-book->new-book [book] (assoc book :authors(set (:authors book))))

(defn has-author? [book author] (contains? (:authors book) author))

(defn authors [books] (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]  (set (map :name (authors books))))

(defn author->string [author] (let [authname (:name author)
                                    years (if (contains? author :death-year)
                                              (str " ("(:birth-year author) " - "(:death-year author)")")
                                              (if (contains? author :birth-year)
                                                  (str " (" (:birth-year author) " - " ")")
                                                  nil))]
                                  (str authname years)))

(defn authors->string [authors] (apply str (interpose ", " (map author->string authors))))

(defn book->string [book] (let [title (:title book)
                                authors (authors->string (:authors book))]
                            (str title ", written by " authors)))

(defn books->string [books] (let [noofbooks (count books)
                                  bookstring (if (> noofbooks 0) (apply str (interpose ", " (map book->string books))))
                                  ]
                              (if  (< noofbooks 1)
                                   (str "No books.")
                                   (if (== noofbooks 1)
                                       (str "1 book. " bookstring ".")
                                       (str noofbooks " books. " bookstring ".")
                                ))))

(defn books-by-author [author books] (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors] (let [result (filter (fn [x] (= (:name x) name)) authors)]
                                         (if (zero?(count result)) nil (first result ))))

(defn living-authors [authors] (filter  alive? authors ))

(defn has-a-living-author? [book] (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books] (filter (fn [x](has-a-living-author? x)) books))

; %________%
