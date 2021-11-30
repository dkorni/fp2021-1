-- Лабораторна робота №4
-- студента групи КН-31 підгрупа 1
-- Варіант 9

--Завдання
--Публiкацiї. Зберiгаються данi про публiкацiї, якi можуть бути книгою (ав-
--тор/спiвавтори, назва, мiсто, видавництво, рiк), статтею (автор/спiвавтори, на-
--зва статтi, назва журналу, рiк, номер журналу, сторiнки) або тезами доповiдi
--(автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто, рiк, сторiнки). Ви-
--значне функцiї для :
--пошуку усiх видавництв, журналiв, конференцiй;

type Author = String
type Commonauthors = [String] 
type Name = String
type City = String
type Publisher = String
type MagazineName = String
type MagazineNumber = Int
type ConferenceName = String
type Year = Int
type Number = Int
type Pages = Int

data Publication = Book Author Commonauthors Name City Publisher Year |  
                   Article Author Commonauthors Name MagazineName Year MagazineNumber Pages |
                   Thesus Author Commonauthors Name ConferenceName City Year Pages
                   deriving Show




findStuff:: [Publication] -> [Publisher]
findStuff [] = []
findStuff ( Book _ _ _ _ n _ :xs) = n : (findStuff xs)
findStuff ( Article _ _ _ n _ _ _ :xs) = n : (findStuff xs)
findStuff ( Thesus _ _ _ n _ _ _ :xs) = n : (findStuff xs)

publications = [
    Book "Werber" [] "Ants" "Moscow" "Geleos" 2007,
    Book "Henry" [] "Selected Stories" "Mojaisk" "Manager" 1992,
    Article "Chloe Tenn" [] "Johnson & Johnson Vaccine Garners First Full Approval" "TheScientist" 2021 243 3, 
    Article "Jonathan O’Callaghan" [] "An ultra-hot gas giant exoplanet orbits its star once every 16 hours" "NewsScientist" 2021 65 3,
    Thesus "Jennifer Aaker" [] "Global Warming" "TED" "New York" 2020 25,
    Thesus "Marc Abrahams" [] "A science award that makes you laugh, then think" "TED" "London" 2021 14
    ]

-- Команда: findStuff publications
-- В результаті дістанемо:
-- ["Geleos","Manager","TheScientist","NewsScientist","TED","TED"]