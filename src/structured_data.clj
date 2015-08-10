(ns structured-data)

(defn do-a-thing [x] (let [x2 (+ x x)]
                       (Math/pow x2 x2)
  ))

(defn spiff [v] (+ (get v 0) (get v 2))
  )

(defn cutify [v] (conj v "<3")
  )

(defn spiff-destructuring [v] (let [[a b c] v]
  (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle] (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)
  ))

(defn square? [rectangle] (if (=(width rectangle) (height rectangle)) true false))


(defn area [rectangle] (* (width rectangle) (height rectangle)) 
  )



(defn contains-point? [rectangle point] (let [[[y1 x1] [y2 x2]] rectangle [yp xp] point ]
                                           (if (and (<= x1 xp x2) (<= y1 yp y2))
                                            true
                                            false)))


(defn contains-rectangle? [outer inner] (let [ [p1 p2] inner] 
                                           
                                          (if (and (contains-point? outer p1) (contains-point? outer p2))
                                                                true
                                                                false))
  )

(defn title-length [book] (count (:title book))
  )

(defn author-count [book] (count (:authors book))
  )

(defn multiple-authors? [book] (if (> (author-count book) 1) true false)
  )

(defn add-author [book new-author] (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author] (if (contains? author :death-year) false true)
  )

(defn element-lengths [collection] (map count collection)
  )

(defn second-elements [collection] (map second collection)
  )

(defn titles [books] (map :title books)
  )

(defn monotonic? [a-seq] (if (or (apply <= a-seq) (apply >= a-seq)) true false)
  )

(defn stars [n] (apply str (repeat n "*"))
  )

(defn toggle [a-set elem] (if (a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq] (if (= (count a-seq) (count (set a-seq))) false true)
  )

(defn old-book->new-book [book] (assoc book :authors (set (:authors book)))
  )


(defn has-author? [book author] (if (nil? ((:authors book) author)) false true)
  )

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn authors [books] (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books] (set (map :name (authors books)))
  )


(defn author->string [author] (if (nil? (:birth-year author)) 
                                (str (:name author))
                                (apply str (vector (:name author) " (" (:birth-year author) " - " (:death-year author) ")"))
  ))

(defn authors->string [authors] (apply str (interpose ", " (map author->string authors)))
  )


(defn book->string [book] (apply str (:title book) ", written by " (authors->string (:authors book)))
  )


(defn books->string [books] (cond
                              (= (count books) 0) "No books."
                              (= (count books) 1) (apply str ["1 book. " (book->string (first books)) "."])
                              :else               (apply str [(count books) " books. " (apply str (interpose ". " (map book->string books))) "."])


  ))

(defn books-by-author [author books] (filter  #(has-author? % author) books)
  )


(defn author-by-name [name authors]  (first (filter #(=(:name %) name)  authors))
  )


(defn living-authors [authors] (filter alive? authors)
  )

(defn has-a-living-author? [book] (let [x (living-authors (:authors book))]
                                    (if (empty? x) false true)
  ))

(defn books-by-living-authors [books] (filter has-a-living-author? books)
  )

; %________%

