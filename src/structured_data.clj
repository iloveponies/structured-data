(ns structured-data)

(defn
  do-a-thing
  "Does sumthing"
  [x]
  (let [express (+ x x)]
    (Math/pow express express))
)


(defn
  spiff
  "Spiff vectors"
  [v]
    (if (< (count v) 3)
     ("?")
     (+ (get v 0) (get v 2))
  )
  )

(defn
  cutify
  "Vector + <3"
  [v]
    (conj v "<3")
)

(defn
  spiff-destructuring
  "Destroy spiff!"
  [v]
     (if (< (count v) 3)
       ("?")
       (let [[a b c] v]
         (+ a c))
     )
)


(defn
  point
  [x y]
  [x y])

(defn
  rectangle
  [bottom-left top-right]
  [bottom-left top-right])

(defn
  width
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
  )
)

(defn
  height
  [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
  )
)

(defn
  square?
  [rect]
  (let [hth (height rect)
        wth (width rect)]
          (= hth wth)
  )
)

(defn
  area
  [rect]
  (let [hth (height rect)
        wth (width rect)]
          (* hth wth)
  )
)

(defn
  contains-point?
  [rectangle point]
  (let [[[botx boty] [topx topy]] rectangle
        [puntx punty] point]
         (and (<= botx puntx topx) (<= boty punty topy))
  )
)


(defn
  contains-rectangle?
  [outer inner]
    (let [[[blix bliy] [tlix tliy]] inner
        [[blox bloy] [tlox tloy]] outer]
          (and (>= blix blox) (>= bliy bloy) (<= tlix tlox) (<= tliy tloy))
  )
)

(defn
  title-length
  [book]
 (count (:title book))
)

(defn
  author-count
  [book]
  (count (:authors book))
)

(defn
  multiple-authors?
  [book]
  (> (author-count book) 1)
)

(defn
  add-author
  [book new-author]
  (let [ababel (:authors book)]
         (assoc book :authors (conj ababel new-author))
      )
)

(defn
  alive?
  [author]
 (not (contains? author :death-year))
)

(defn
  element-lengths
  [collection]
  (map count collection)
  )

(defn
  second-elements
  [collection]
    (let [funge (fn [v] (get v 1))]
      (map funge collection)
    )
)

(defn
  titles
  [books]
    (map :title books)
  )

(defn
  monotonic?
  [a]
  (or
    (apply <= a)
    (apply >= a)
  )
)

(defn
  stars
  [n]
  (let [aaa (seq (repeat n "*"))]
    (apply str aaa)
  )
)

(defn
  toggle
  [a e]
  (if (contains? a e)
    (disj a e)
    (conj a e))
)

(defn
  contains-duplicates?
  [a-seq]
  (let [abel a-seq
        babel (count abel)
        cabel (set abel)
        dabel (count cabel)
        ]
          (not (= babel dabel))
  )
)

(defn
  old-book->new-book
  [book]
  (let [fabel (set (:authors book))]
         (assoc book :authors fabel)
  )
)

(defn
  has-author?
  [book author]
  (let [gabel (:authors book)]
         (contains? gabel author)
  )
)


(defn
  authors
  [books]
   (apply clojure.set/union (map :authors books))
  )

(defn
  all-author-names
  [books]
   (set (map :name (authors books)))
  )

(defn
  author->string
  [author]
  (let [habel (:name author)
        jabel (:birth-year author)
        kabel (:death-year author)]
          (if (contains? author :birth-year)
            (str habel " (" jabel " - " kabel ")")
            (str habel)
          )
  )
)

(defn
  authors->string
  [authors]
   (let [label (map  author->string authors)
          mabel (apply str (interpose ", " label))
          ]
           mabel
      )
)

(defn
  book->string
  [book]
  (let [nabel (authors->string (:authors book))
        obel (:title book)
        pabel (str obel  ", written by " nabel)
        ]
          pabel
  )

)

(defn
  books->string
  [books]
  (let [rabel ""
        qabel (count books)
        sabel (map  book->string books)
        tabel (apply str (interpose ". " sabel))]
            (cond
              (= qabel 0) (str rabel "No books." tabel)
              (= qabel 1) (str rabel "1 book. " tabel ".")
              (> qabel 1) (str rabel qabel " books. " tabel ".")
             )
    )
)

(defn
  books-by-author
  [author books]
  (let [vabel (filter  (fn [book] (contains? (:authors book) author)) books)]
      vabel
      )
  )

(defn
  author-by-name
  [name authors]
    (let [wabel (filter (fn [x] (= name (:name x))) authors)]
    (first wabel)
  )
)

(defn
  living-authors
  [authors]
  (let [xabel (filter (fn [x] (alive? x)) authors)]
    (if (empty? xabel)
      ()
      (seq xabel)
      )
    )
)

(defn
  has-a-living-author?
  [book]
  (let [zabel (authors [book])
      yabel (living-authors zabel)]
        (not (empty? yabel))
  )
)

(defn
  books-by-living-authors
  [books]
    (let [aabel (filter (fn [x] (has-a-living-author? x)) books)]
    aabel
    )
  )

; X________X
