(ns structured-data)


;; done
(defn do-a-thing [x]
  (let [mything (+ x x)]
    (Math/pow mything mything)
  )
)

;; done
(defn spiff [v]
  (+ (get v 0) (get v 2))
)

;; done
(defn cutify [v]
  (assoc v (count v) "<3")
)

;; done
(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
    )
)

;; done
(defn point [x y]
  [x y])
(defn rectangle [bottom-left top-right]
  [bottom-left top-right])
(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )


;; done
(defn square? [rectangle]
  (== (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
       (and (<= x1 x x2) (<= y1 y y2))
  ))
  )

(defn contains-rectangle? [outer inner]
  (let [[left right] inner]
    (and (contains-point? outer left)
         (contains-point? outer right))
    )
  )


;; done
(defn title-length [book]
  (count (:title book))
  )

;; done
(defn author-count [book]
  (count (:authors book))
  )

;; done
(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

;; done
(defn add-author [book new-author]
    (let [authors (:authors book)]
      (assoc book :authors (conj authors new-author))
      )
)


;; done
(defn alive? [author]
  (not (contains? author :death-year))
)


;; done
(defn element-lengths [collection]
  (map count collection)
  )

;; done
(defn second-elements [collection]
  (let[getsecond (fn [v] (get v 1))]
    (map getsecond collection)
    )
  )

;; done
(defn titles [books]
  (let [get_title (fn [book] (get book :title))]
    (map get_title books)
  )
)

;; done
(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq))
)

;;done
(defn stars [n]
  (apply str (repeat n "*"))
)

;; done
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

;; done
(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq)))
)

;; done
(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))
    )
  )


;; done
(defn has-author? [book author]
  (contains? (get book :authors) author)
  )

;; done
(defn authors [books]
  (let [get_authors (fn [book] (:authors book))]
    (let [all_authors (map get_authors books)]
      (apply clojure.set/union all_authors)
      )
    )
  )

;; done
(defn all-author-names [books]
  (set (map :name (authors books)))
  )

;; done
(defn author->string [author]
  (let[name (:name author)]
    (let[[by dy] [(:birth-year author) (:death-year author)]]
      (if by
        (str name " (" by " - "dy ")")
        (str name)
        )
      )
    )
  )

;;done
(defn authors->string [authors]
  (let [author_v (map author->string authors)]
    (apply str (interpose ", " author_v))
    )
  )

;; DONE
(defn book->string [book]
  (let [name (:title book)]
    (let [authors (authors->string (:authors book))]
      (str name ", written by " authors)
      )
    )
  )

  ;; DONE
(defn books->string [books]
  (let [n (count books)]
    (let [book_strs (map book->string books)]
      (let [book_info (apply str (interpose ". " book_strs))]
        (let  [howmany (cond
                         (= 0 n) "No books"
                         (> n 1) (str n " books. ")
                         (= n 1) (str n " book. "))]
        (str howmany book_info ".")
          )
        )
      )

    )
  )


;; done
(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )


;; done
(defn author-by-name [name authors]
  (let [matches (filter (fn [author]
                        (= (:name author) name))
                        authors)]

    (first matches)
    )
  )


;; done
(defn living-authors [authors]
  (filter (fn[author] (alive? author)) authors)
  )



(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (let [living (living-authors authors)]
      (not (empty? living))
      )
    )
  )


(defn books-by-living-authors [books]
  (filter (fn[book] (has-a-living-author? book)) books)
  )


; %________%
