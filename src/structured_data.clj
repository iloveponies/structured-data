(ns structured-data)

(defn do-a-thing 
	"Tuplataan syöte"
	[x]  
	(let [tupla (+ x x)]
		(Math/pow tupla tupla)) )

(defn spiff 
	"Haetaan 1. ja 3. elementti vektorista"
	[v]  
	(+ (get v 0) (get v 2)) )

(defn cutify 
	"Lisätään lempeä vektoriin"
	[v]  
	(conj v "<3") )

(defn spiff-destructuring 
	"Poistetaan viimeinen elementti vektorista"
	[v] 
 	(let [[x y z] v]
		(+ x z)) )

(defn point [x y] 
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width 
	"Lasketaan suorakaiteen leveys"
	[rectangle] 
 	 (let [[[x1 y1] [x2 y2]] rectangle]
		(- x2 x1)) )

(defn height 
	"Lasketaan suorakaiteen korkeus"
	[rectangle] 
	(let [[[x1 y1] [x2 y2]] rectangle]
                (- y2 y1)) )

(defn square? 
	"Tutkitaan onko suorakaide neliö"
	[rectangle] 
	 (let [[[x1 y1] [x2 y2]] rectangle]
		(if (== (- y2 y1) (- x2 x1)) true false)) )

(defn area 
	"Lasketaan suorakaiteen pinta-ala"
	[rectangle] 
	(let [[[x1 y1] [x2 y2]] rectangle]     
                 (* (- y2 y1) (- x2 x1))) )

(defn contains-point? 
	"Sisältyykö annettu piste suorakaiteeseen"
	[rectangle point] 
	(let [[[x1 y1] [x2 y2]] rectangle
		[px py] point]
		(if (and (<= x1 px x2) (<= y1 py y2)) true false)) )

(defn contains-rectangle? 
	"Sisältyykö inner outeriin"
	[outer inner] 
	(let [[[x1 y1] [x2 y2]] inner]
		(if (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2])) true false)) )

(defn title-length 
	"Kirjan nimen pituus merkkeinä"
	[book] 
	(count (:title book)) )

(defn author-count 
	"Tekijöiden lukumäärä"
	[book] 
  	(count (:authors book)) )

(defn multiple-authors? 
	"Onko tekijöitä useampia"
	[book] 
  	(if (< 1 (count (:authors book))) true false) )

(defn add-author 
	"Uudet tekijän lisääminen"
	[book new-author] 
	(let [authors (:authors book)
		new (conj authors new-author)]
		(assoc book :authors new)) )

(defn alive? 
	"Onko tekijä elossa"
	[author] 
	(not (contains? author :death-year)) )

(defn element-lengths 
	"Kokoelman koko"
	[collection] 
  	(map count collection) )

(defn second-elements 
	"Poimitaan kokoelman kokoelmista toiseksi indeksoituvat elementit ja kootaan uusi kokoelma niistä"
	[collection] 
	(let [toinen (fn [x] (get x 1))]
		(map toinen collection)) )

(defn titles 
	"Kootan kirjojen otsikot"
	[books]  
	(map :title books) )

(defn monotonic? 
	"Onko sarja monotoninen"
	[a-seq] 
	(or (apply >= a-seq) (apply <= a-seq)) )

(defn stars 
	"Tehdään tähtiä"
	[n] 
	(apply str (repeat n "*")) )

(defn toggle 
	"päälle/pois"
	[a-set elem] 
	(if (contains? a-set elem)
		(disj a-set elem)
		(conj a-set elem)) )

(defn contains-duplicates? 
	"Onko sarjassa duplikaatteja"
	[a-seq] 
	(let [set (set a-seq)]
		(not= (count set) (count a-seq))) )

(defn old-book->new-book 
	"Vaihdetaan kirjan muotoilua"
	[book] 
	 (let [set (set (:authors book))]
		(assoc book :authors set)) )

(defn has-author? 
	"Onko kirjalla annettu tekijä"
	[book author] 
	(contains? (:authors book) author) )

(defn authors 
	"Listataan kaikki tekijät"
	[books] 
	(apply clojure.set/union (map :authors books)) )

(defn all-author-names 
	"Listataan tekijöiden nimet - tää ei vielä ihan uponnut"
	[books] 
	(let [author-names
		(fn [book] (map :name (:authors book)))]
	(set (apply concat (map author-names books)))) )

(defn author->string 
	"Tekijä lukukelpoiseen muotoon"
	[author] 
	(let [name (:name author)
		birth (:birth-year author)
		death (:death-year author)]
		(cond
			(not birth) (str name)
			:else  (str name " (" birth " - " death ")"))) )

(defn authors->string 
	"Useampi tekijä lukukelpoiseen muotoon"
	[authors] 
	(apply str (interpose ", " (map author->string authors))) )

(defn book->string 
	"Kirja lukukelpoiseen muotoon"
	[book] 
	(str (:title book) ", written by " (authors->string (:authors book))) )

(defn books->string 
	"Useampi kirja lukukelpoiseen muotoon"
	[books] 
	(let [kirjalista (apply str (interpose ". " (map book->string books)))]
		(cond
			(<= (count books) 0) (str "No books.")
			(== (count books)  1) (str "1 book. " kirjalista ".")
			:else (str (count books) " books. " kirjalista "."))) )

(defn books-by-author 
	"Kirjat annetulta tekijältä"
	[author books] 
	(filter (fn [x] (has-author? x author )) books) )

(defn author-by-name 
	"Etsii nimeä tekijöistä"
	[name authors] 
	(let [found? (fn [x] (= (:name x) name))]
	  (first(filter found? authors))))

(defn living-authors 
	"Etsii elossa olevat tekijät joukosta"
	[authors] 
	(filter alive? authors) )

(defn has-a-living-author? 
	"Tutkii onko joku kirjan tekijöistä elossa"
	[book] 
	(< 0 (count (living-authors (:authors book)))) )

(defn books-by-living-authors 
	"Kirjat joilla on elossa olevia tekijöitä"
	[books] 
	(filter (fn [book] (has-a-living-author? book)) books) )

; %________%
