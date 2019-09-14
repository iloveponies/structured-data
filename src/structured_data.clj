(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)] (Math/pow xx xx)))

(defn spiff [v]
  (let [f (or (get v 0) 0)
        t (or (get v 2) 0)]
    (+ f t)
    ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[f _ t] v]
    (+ f t)
    ) )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))
    )
)

(defn contains-rectangle? [outer inner]
  (let [[[xo1 yo1] [xo2 yo2]] outer
        [[xi1 yi1] [xi2 yi2]] inner]
    (and (<= xo1 xi1 xi2 xo2) (<= yo1 yi1 yi2 yo2))
    )
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [auths (:authors book)
        nauths (conj auths new-author)]
    (assoc book :authors nauths)
    ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [v] (get v 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)] (not (= (count a-set) (count a-seq))) ))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [ausets (map :authors books)]
    (apply clojure.set/union ausets)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]

    (cond 
     (not (and (nil? birth) (nil? death))) (str name " (" birth " - " death ")" )
     (not (and (nil? birth))) (str name " (" birth " - )")
     :default name)
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (empty? books) "No books."
   (= 1 (count books)) (str "1 book. "  (apply book->string books) ".")
   :default (let [ bookss (map book->string books)
                 ]
              (str (count bookss)
                   " books. "
                    (apply str (doall (interpose ". " bookss)))
                   ".")
              )))

(defn books-by-author [author books]
  (filter #(contains? (:authors %) author) books))

(defn author-by-name [name authors]
  (first  (filter #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
