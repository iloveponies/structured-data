(ns structured-data)

(defn do-a-thing [x]
  (let [n (+ x x)]
    (Math/pow n n))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )
; too short vector -> NullPointerException

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x nic y] v]
    (+ x y))
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
   (let
    [
     [[x1 y1] [x2 y2]] rectangle
     h (- x2 x1)
    ]
    (if (< h 0) (* h -1) h)
  )
)

(defn height [rectangle]
  (let
    [
     [[x1 y1] [x2 y2]] rectangle
     h (- y2 y1)
    ]
    (if (< h 0) (* h -1) h)
  )
)

(defn square? [rectangle]
  (let
    [
      h (height rectangle)
      w (width rectangle)
     ]
    (if (= h w) true false)
    )
)

(defn area [rectangle]
 (let
    [
      h (height rectangle)
      w (width rectangle)
     ]
    (* h w)
    )
)


(defn contains-point? [rectangle point]
  (let
    [
     [[x1 y1] [x2 y2]] rectangle
      [p1 p2] point
     ]
    (if (and (<= x1 p1 x2) (<= y1 p2 y2)) true false)
  )
)

(defn contains-rectangle? [outer inner]
  ; vselijaky moznosti prirazeni bodu
  (let
    [[[ix1 iy1] p2] inner]
    (and (contains-point? outer [ix1 iy1]) (contains-point? outer p2))
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (> (count (:authors book)) 1)
)

(defn add-author [book new-author]
    ; k map book na key authors priradim vector rozsireny (conj)
    ; o noveho autora
    ; do assoc pouziju originalni jmeno aby to nebylo lokalni jen
    ; a taky to nefunguje :D
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))
  )
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (let [pom (fn [x] (count x))]
  (map pom collection))
)

(defn second-elements [collection]
  (let [pom (fn [x] (get x 1))]
    (map pom collection)
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (if (apply <= a-seq) true (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (let [new_set (set a-seq)]
    (not (= (count new_set) (count a-seq))))
)

(defn old-book->new-book [book]
  (let [old_authors (:authors book)]
    (assoc book :authors (set old_authors))
    )
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (let [names (fn [x] (:authors x))]
    (apply clojure.set/union (map names books))
  )
)

(defn all-author-names [books]
  (let [names (fn [x] (map :name (:authors x)))]
      (set (apply clojure.set/union (map names books)))
    ; below not working, map vraci sequence, union to umi spojit
    ; set z toho pak udela mnozinu
;;       (apply clojure.set/union (set (map names books)))
    )
)

(defn author->string [author]
  (let [
        name (str (:name author))
        has_date (contains? author :birth-year)
        ]
    (if has_date
      (str name " (" (:birth-year author) " - "
           (if (contains? author :death-year) (:death-year author) "") ")") name)
    )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
)

(defn books->string [books]
  (let [
        cnt (count books)
        ]
    (str (cond
     (= cnt 0) "No books."
     (= cnt 1) "1 book. "
     :else (str cnt " books. ")
     )
    (if (> cnt 0 )(str (apply str (interpose ". " (map book->string books))) "."))
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
)

(defn author-by-name [name authors]
  (let [result (filter (fn [x] (= name (:name x))) authors)]
    (if (= 0 (count result)) nil (first result))
    )
)

(defn living-authors [authors]
  (filter alive? authors)
)

(defn has-a-living-author? [book]
  (let [living_authors (filter alive? (:authors book))]
    (if (= 0 (count living_authors)) false true)
    )
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)
