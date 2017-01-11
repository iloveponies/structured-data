(ns structured-data)

(defn do-a-thing [x] (let[x (+ x x)]
                       (Math/pow x x)))

(defn spiff [v](let[x (get v 0) y (get v 2)]
                 (+ x y)
                 )
  )

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v]
  (let[[x y z] v]
    (+ x z)
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle](let[[[x1 y1][x2 y2]]rectangle]
                         (- x2 x1)
                         ))

(defn height [rectangle](let[[[x1 y1][x2 y2]]rectangle]

                          (- y2 y1)
                          )
  )

(defn square? [rectangle](if(== (height rectangle)(width rectangle))true false
                          )
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point](let[[[x1 y1][x2 y2]] rectangle[x3 y3] point]
                                         (if(and(<= x1 x3 x2)(<= y1 y3 y2))true false)
                                         )
  )

(defn contains-rectangle? [outer inner](let[[[r1x1 r1y1][r1x2 r1y2]] outer[[r2x1 r2y1][r2x2 r2y2]]inner]
                                         (if(and(<= r1x1 r2x1 r2x2 r1x2)(<= r1y1 r2y1 r2y2 r1y2))true false)
                                         )
  )

(defn title-length [book]
  (count (:title book))
)
(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (> (count(:authors book)) 1)
  )

(defn add-author [book new-author]
  (let[authors (:authors book)]
    (assoc book :authors(conj authors new-author)))

  )

(defn alive? [author] (not(contains? author :death-year)))

(defn element-lengths [collection](map count collection))

(defn second-elements [collection](let[asd(fn[x](get x 1))]
                                    (map asd collection)))

(defn titles [books](map :title books))

(defn monotonic? [a-seq](or(apply <= a-seq)(apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem)(disj a-set elem)(conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(== (count a-seq)(count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books] (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author](let[name(:name author)birth-year(:birth-year author)death-year(:death-year author)]
                               (if birth-year
                                 (str name " (" (str birth-year) " - " (str death-year) ")")
                                 (str name))))

(defn authors->string [authors]
  (apply str(interpose ", "(map author->string authors))))

(defn book->string [book](let[title(:title book)authors(:authors book)]
                           (str title ", written by " (authors->string authors))))

(defn books->string [books](if(empty? books)
                             (str "No books.")
                             (if(= 1 (count books))
                               (str "1 book. " (apply str (interpose ". " (map book->string books)))".")
                               (str (count books) " books. "(apply str (interpose ". " (map book->string books)))".")
                               )
                             )
  )

(defn books-by-author [author books](filter (fn[book](has-author? book author)) books))

(defn author-by-name [name authors]
  (first(filter (fn[author](= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors(:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
