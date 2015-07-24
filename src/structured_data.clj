(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)
        x2 (+ x x)]

 (Math/pow x2 xx))
)


(defn spiff [v]
  (cond
  (number? (get v 2))
  (let [a (get v 0)
        b (get v 2)]
    (+ a b))
   :else nil))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
   (if (> (count v) 2)
   (let [a (get v 0)
        b (get v 2)]
      (+ a b)
      )
     nil
))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 0)
        d(get b 0)]
        (- d c)
      )
)


  (defn height [rectangle]
  (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 1)
        d(get b 1)]
    (- d c)
      )
)

(defn square? [rectangle]
   (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 0)
        d(get b 0)
        e (get a 1)
        f(get b 1)
         cd (* d c)
         ef (* e f)]
      (if (= cd ef)
    true
    (if(= cd 0)
      true
      (if (= ef 0)
      true
      false)
      ))))

(defn area [rectangle]
     (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 0)
        d(get b 0)
        e (get a 1)
        f(get b 1)
         cd (- c d)
         ef (- e f)]
       (* cd ef)
      ))


(defn contains-point? [rectangle point]
(let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 0)
        d(get b 0)
        e (get a 1)
        f(get b 1)
      eka (get point 0)
      toka (get point 1)]
      (if (and (and (and (<= eka c)(<= toka e)(>= eka 0)(>= toka 0))))
    true
    (if(and (and (and (<= eka d) (<= toka f)(>= eka 0)(>= toka 0))))
      true
      false)
    )))


(defn contains-rectangle? [outer inner]
  (if(=(contains-point? outer (get inner 0)) true)
    true
    (contains-point? outer (get inner 1)))
)


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (== (author-count book) 1)
    false
    true)
  )


(defn add-author [book new-author]
  (let [orgAuth (book :authors)
        newAuth (conj orgAuth new-author)
        newBook {:title (book :title)
                     :authors newAuth}
        newBook2 {:authors newAuth}]
    (if (> (title-length newBook) 0)
      newBook
      newBook2
    )
))




(defn alive? [author]
  (let[death(contains? author :death-year)
       really(if (= death true)
         false
         true)]
    really))

(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
