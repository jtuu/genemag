open Feed_tables;;

let find_index haystack needle =
  let len = Array.length haystack in
  let rec search i =
    if i >= len then -1
    else if haystack.(i) == needle then i
    else search (i + 1) in
  search 0;;

exception InvalidEnum of string;;

type mag_type =
  | None
  | Mag
  | Varuna
  | Kalki
  | Vritra 
  | Rudra
  | Marutah
  | Vayu
  | Surya
  | Mitra
  | Tapas
  | Sumba
  | Ashvinau
  | Namuci 
  | Andhaka
  | Apsaras
  | Bana
  | Bhirava
  | Durga
  | Garuda
  | Ila
  | Kabanda
  | Kaitabha
  | Kama
  | Kumara
  | Madhu
  | Marica
  | Naga
  | Nandin
  | Naraka
  | Ravana
  | Ribhava
  | Sita
  | Soma
  | Ushasu
  | Varaha
  | Yaksa 
  | Bhima
  | Deva
  | Diwari
  | Nidra
  | Pushan
  | Rati
  | Rukmin
  | Sato
  | Savitri 
  | Agastya
  | AngelsWing
  | Chao
  | ChuChu
  | Churel
  | DevilsTail
  | DevilsWing
  | Dreamcast
  | Elenor
  | GaelGiel
  | Genesis
  | Geungsi
  | Hamburger
  | KapuKapu
  | MarkIII
  | MasterSystem
  | Moro
  | OpaOpa
  | PanzersTail
  | Pian
  | Pioneer
  | Pitri
  | Preta
  | Puyo
  | RoboChao
  | Rappy
  | SegaSaturn
  | Soniti
  | StrikerUnit
  | Tellusis
  | Yahoo;;

let mag_name = function
  | Mag -> "Mag"
  | Varuna -> "Varuna"
  | Kalki -> "Kalki"
  | Vritra -> "Vritra" 
  | Rudra -> "Rudra"
  | Marutah -> "Marutah"
  | Vayu -> "Vayu"
  | Surya -> "Surya"
  | Mitra -> "Mitra"
  | Tapas -> "Tapas"
  | Sumba -> "Sumba"
  | Ashvinau -> "Ashvinau"
  | Namuci -> "Namuci" 
  | Andhaka -> "Andhaka"
  | Apsaras -> "Apsaras"
  | Bana -> "Bana"
  | Bhirava -> "Bhirava"
  | Durga -> "Durga"
  | Garuda -> "Garuda"
  | Ila -> "Ila"
  | Kabanda -> "Kabanda"
  | Kaitabha -> "Kaitabha"
  | Kama -> "Kama"
  | Kumara -> "Kumara"
  | Madhu -> "Madhu"
  | Marica -> "Marica"
  | Naga -> "Naga"
  | Nandin -> "Nandin"
  | Naraka -> "Naraka"
  | Ravana -> "Ravana"
  | Ribhava -> "Ribhava"
  | Sita -> "Sita"
  | Soma -> "Soma"
  | Ushasu -> "Ushasu"
  | Varaha -> "Varaha"
  | Yaksa -> "Yaksa" 
  | Bhima -> "Bhima"
  | Deva -> "Deva"
  | Diwari -> "Diwari"
  | Nidra -> "Nidra"
  | Pushan -> "Pushan"
  | Rati -> "Rati"
  | Rukmin -> "Rukmin"
  | Sato -> "Sato"
  | Savitri -> "Savitri" 
  | Agastya -> "Agastya"
  | AngelsWing -> "Angel's Wing"
  | Chao -> "Chao"
  | ChuChu -> "Chu Chu"
  | Churel -> "Churel"
  | DevilsTail -> "Devil's Tail"
  | DevilsWing -> "Devil's Wing"
  | Dreamcast -> "Dreamcast"
  | Elenor -> "Elenor"
  | GaelGiel -> "Gael Giel"
  | Genesis -> "Genesis"
  | Geungsi -> "Geung-si"
  | Hamburger -> "Hamburger"
  | KapuKapu -> "Kapu Kapu"
  | MarkIII -> "Mark III"
  | MasterSystem -> "Master System"
  | Moro -> "Moro"
  | OpaOpa -> "Opa-Opa"
  | PanzersTail -> "Panzer's Tail"
  | Pian -> "Pian"
  | Pioneer -> "Pioneer"
  | Pitri -> "Pitri"
  | Preta -> "Preta"
  | Puyo -> "Puyo"
  | RoboChao -> "RoboChao"
  | Rappy -> "Rappy"
  | SegaSaturn -> "Sega Saturn"
  | Soniti -> "Soniti"
  | StrikerUnit -> "Striker Unit"
  | Tellusis -> "Tellusis"
  | Yahoo -> "Yahoo!"
  | None -> "INVALID";;

type photon_blast =
  | None
  | Twins
  | Golla
  | Pilla
  | Estlla
  | Leilla
  | Farlla;;

let mag_photon_blast_max_amount = 3;;

type mag = {
    stats : int array;
    level : int;
    photon_blasts : photon_blast array;
    feed_table : int;
    mag_type : mag_type;
    evolution_tier : int;
  };;

type food =
  | Monomate
  | Dimate
  | Trimate
  | Monofluid
  | Difluid
  | Trifluid
  | Antidote
  | Antiparalysis
  | SolAtomizer
  | MoonAtomizer
  | StarAtomizer
  | Last;;

let food_name = function
  | Monomate -> "Monomate"
  | Dimate -> "Dimate"
  | Trimate -> "Trimate"
  | Monofluid -> "Monofluid"
  | Difluid -> "Difluid"
  | Trifluid -> "Trifluid"
  | Antidote -> "Antidote"
  | Antiparalysis -> "Antiparalysis"
  | SolAtomizer -> "Sol Atomizer"
  | MoonAtomizer -> "Moon Atomizer"
  | StarAtomizer -> "Star Atomizer"
  | Last -> "INVALID";;

let food_index = function
  | Monomate -> 0
  | Dimate -> 1
  | Trimate -> 2
  | Monofluid -> 3
  | Difluid -> 4
  | Trifluid -> 5
  | Antidote -> 6
  | Antiparalysis -> 7
  | SolAtomizer -> 8
  | MoonAtomizer -> 9
  | StarAtomizer -> 10
  | Last -> 11;;

let food_of_index = function
  | 0 -> Monomate
  | 1 -> Dimate
  | 2 -> Trimate
  | 3 -> Monofluid
  | 4 -> Difluid
  | 5 -> Trifluid
  | 6 -> Antidote
  | 7 -> Antiparalysis
  | 8 -> SolAtomizer
  | 9 -> MoonAtomizer
  | 10 -> StarAtomizer
  | 11 -> Last
  | _ -> raise (InvalidEnum "Invalid food index");;

type feed_stat =
  | Def
  | Pow
  | Dex
  | Mind
  | Synchro
  | Iq;;

let stat_index = function
  | Def -> 0
  | Pow -> 1
  | Dex -> 2
  | Mind -> 3
  | Synchro -> 4
  | Iq -> 5;;

let stat_of_index = function
  | 0 -> Def
  | 1 -> Pow
  | 2 -> Dex
  | 3 -> Mind
  | 4 -> Synchro
  | 5 -> Iq
  | _ -> raise (InvalidEnum "Invalid stat index");;

let stat_name = function
  | Def -> "DEF"
  | Pow -> "POW"
  | Dex -> "DEX"
  | Mind -> "MIND"
  | Synchro -> "Synchro"
  | Iq -> "IQ";;

let stat_max_value = function
  | Def -> 20000
  | Pow -> 20000
  | Dex -> 20000
  | Mind -> 20000
  | Synchro -> 120
  | Iq -> 200;;

type section_id_group =
  | None
  | Any
  | GBPOW
  | VSPRY
  | GPO
  | SPY
  | VBRW;;

type gender =
  | Any
  | Male
  | Female;;

type character_class =
  | Any
  | Hu
  | Ra
  | Fo;;

type section_id =
  | None
  | Bluefull
  | Greenill
  | Oran
  | Pinkal
  | Purplenum
  | Redria
  | Skyly
  | Viridia
  | Yellowboze
  | Whitill;;

let section_id_in_group (sec_id : section_id) (grp : section_id_group) : bool =
  if grp == Any then true
  else if grp == None then false
  else Array.exists (fun s -> sec_id == s)
                    (match grp with
                     | GBPOW -> [|Greenill; Bluefull; Pinkal; Oran; Whitill|]
                     | VSPRY -> [|Viridia; Skyly; Purplenum; Redria; Yellowboze|]
                     | GPO -> [|Greenill; Purplenum; Oran|]
                     | SPY -> [|Skyly; Pinkal; Yellowboze|]
                     | VBRW -> [|Viridia; Bluefull; Redria; Whitill|]
                     | _ -> [|None|]);;

type feeder = {
    section_id : section_id;
    gender : gender;
    character_class : character_class;
  };;

type mag_evolution = {
    mag_type : mag_type;
    evolution_tier : int;
    photon_blast : photon_blast;
    feed_table : int;
    mag_condition : mag_type;
    stat_condition : int array -> bool;
    section_id_condition : section_id_group;
    level_condition : int;
    gender_condition : gender;
    class_condition : character_class;
  };;

let no_stat_condition _ = true;;

let stat_condition_greater_than_others greatest_stat stats =
  let greatest_val = stats.(stat_index greatest_stat) in
  let greatest_index = stat_index greatest_stat in
  let stats_length = Array.length stats in
  let rec all_less_than i =
    if i >= stats_length then true
    else if i == greatest_index || stats.(i) < greatest_val then
      (all_less_than (i + 1))
    else false in
  all_less_than 0;;

let evolutions = [|
    {
      mag_type = Varuna;
      evolution_tier = 1;
      photon_blast = Farlla;
      feed_table = 1;
      mag_condition = Mag;
      stat_condition = no_stat_condition;
      section_id_condition = Any;
      level_condition = 10;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Kalki;
      evolution_tier = 1;
      photon_blast = Estlla;
      feed_table = 1;
      mag_condition = Mag;
      stat_condition = no_stat_condition;
      section_id_condition = Any;
      level_condition = 10;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Vritra;
      evolution_tier = 1;
      photon_blast = Leilla;
      feed_table = 1;
      mag_condition = Mag;
      stat_condition = no_stat_condition;
      section_id_condition = Any;
      level_condition = 10;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Rudra;
      evolution_tier = 2;
      photon_blast = Golla;
      feed_table = 2;
      mag_condition = Varuna;
      stat_condition = stat_condition_greater_than_others Pow;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Marutah;
      evolution_tier = 2;
      photon_blast = Pilla;
      feed_table = 2;
      mag_condition = Varuna;
      stat_condition = stat_condition_greater_than_others Dex;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Vayu;
      evolution_tier = 2;
      photon_blast = Twins;
      feed_table = 4;
      mag_condition = Varuna;
      stat_condition = stat_condition_greater_than_others Mind;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Surya;
      evolution_tier = 2;
      photon_blast = Golla;
      feed_table = 3;
      mag_condition = Kalki;
      stat_condition = stat_condition_greater_than_others Pow;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Mitra;
      evolution_tier = 2;
      photon_blast = Golla;
      feed_table = 3;
      mag_condition = Kalki;
      stat_condition = stat_condition_greater_than_others Dex;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Tapas;
      evolution_tier = 2;
      photon_blast = Twins;
      feed_table = 3;
      mag_condition = Kalki;
      stat_condition = stat_condition_greater_than_others Mind;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Sumba;
      evolution_tier = 2;
      photon_blast = Golla;
      feed_table = 2;
      mag_condition = Vritra;
      stat_condition = stat_condition_greater_than_others Pow;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Ashvinau;
      evolution_tier = 2;
      photon_blast = Pilla;
      feed_table = 2;
      mag_condition = Vritra;
      stat_condition = stat_condition_greater_than_others Dex;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Namuci;
      evolution_tier = 2;
      photon_blast = Twins;
      feed_table = 2;
      mag_condition = Vritra;
      stat_condition = stat_condition_greater_than_others Mind;
      section_id_condition = Any;
      level_condition = 35;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Andhaka;
      evolution_tier = 3;
      photon_blast = Estlla;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) >= 45
                                     && stat_condition_greater_than_others Mind stats);
      section_id_condition = Any;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Apsaras;
      evolution_tier = 3;
      photon_blast = Estlla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) >= stats.(stat_index Mind)
                                     && stats.(stat_index Mind) > stats.(stat_index Dex));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Apsaras;
      evolution_tier = 3;
      photon_blast = Estlla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) > stats.(stat_index Dex)
                                     && stats.(stat_index Dex) > stats.(stat_index Pow));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Bana;
      evolution_tier = 3;
      photon_blast = Estlla;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) > stats.(stat_index Pow)
                                     && stats.(stat_index Pow) >= stats.(stat_index Dex));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Bana;
      evolution_tier = 3;
      photon_blast = Estlla;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) >= 45
                                     && (stats.(stat_index Dex) >= stats.(stat_index Pow)
                                         || stats.(stat_index Mind) >= stats.(stat_index Pow)));
      section_id_condition = Any;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Bhirava;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) >= stats.(stat_index Mind)
                                     && stats.(stat_index Mind) > stats.(stat_index Dex));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Bhirava;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> (stats.(stat_index Pow) > stats.(stat_index Mind)
                                      && stats.(stat_index Mind) > stats.(stat_index Dex))
                                     || (stats.(stat_index Dex) >= stats.(stat_index Pow)
                                      && stats.(stat_index Pow) > stats.(stat_index Mind)));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Bhirava;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Mind)
                                     && stats.(stat_index Mind) >= stats.(stat_index Pow));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Durga;
      evolution_tier = 3;
      photon_blast = Estlla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) > stats.(stat_index Dex)
                                     && stats.(stat_index Dex) > stats.(stat_index Pow));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Garuda;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Pow)
                                     && stats.(stat_index Pow) > stats.(stat_index Mind));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Garuda;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Pow)
                                     && stats.(stat_index Pow) > stats.(stat_index Mind));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Garuda;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Pow)
                                     && stats.(stat_index Pow) > stats.(stat_index Mind));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Ila;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Pow)
                                     && stats.(stat_index Pow) > stats.(stat_index Mind));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Ila;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) >= stats.(stat_index Dex)
                                     && stats.(stat_index Dex) > stats.(stat_index Pow));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Kabanda;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) > stats.(stat_index Pow)
                                     && stats.(stat_index Pow) >= stats.(stat_index Dex));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Kabanda;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) > stats.(stat_index Pow)
                                     && stats.(stat_index Pow) >= stats.(stat_index Dex));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Kabanda;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) >= stats.(stat_index Dex)
                                     && stats.(stat_index Dex) > stats.(stat_index Pow));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Kaitabha;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) > stats.(stat_index Mind)
                                     && stats.(stat_index Mind) > stats.(stat_index Dex));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Kaitabha;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) >= stats.(stat_index Pow)
                                     && stats.(stat_index Pow) > stats.(stat_index Mind));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Kama;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) >= stats.(stat_index Dex)
                                     && stats.(stat_index Dex) >= stats.(stat_index Mind));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Kama;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) == stats.(stat_index Mind)
                                     && stats.(stat_index Mind) > stats.(stat_index Pow));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Kama;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> (stats.(stat_index Pow) > stats.(stat_index Dex)
                                      && stats.(stat_index Dex) >= stats.(stat_index Mind)
                                      || (stats.(stat_index Dex) >= stats.(stat_index Mind)
                                          && stats.(stat_index Mind) >= stats.(stat_index Pow))
                                      || (stats.(stat_index Pow) == stats.(stat_index Mind)
                                          && stats.(stat_index Mind) > stats.(stat_index Dex))));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Kumara;
      evolution_tier = 3;
      photon_blast = Golla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> (stats.(stat_index Mind) >= stats.(stat_index Pow)
                                      && stats.(stat_index Pow) >= stats.(stat_index Dex))
                                     || (stats.(stat_index Pow) == stats.(stat_index Dex)
                                         && stats.(stat_index Dex) > stats.(stat_index Mind)));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Madhu;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) > stats.(stat_index Dex)
                                      && stats.(stat_index Dex) >= stats.(stat_index Mind));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Marica;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) > stats.(stat_index Dex)
                                      && stats.(stat_index Dex) >= stats.(stat_index Mind));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Naga;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) > stats.(stat_index Mind)
                                      && stats.(stat_index Mind) > stats.(stat_index Dex));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Naga;
      evolution_tier = 3;
      photon_blast = Twins;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> (stats.(stat_index Mind) >= stats.(stat_index Pow)
                                      && stats.(stat_index Pow) >= stats.(stat_index Dex))
                                     || (stats.(stat_index Pow) == stats.(stat_index Dex)
                                         && stats.(stat_index Dex) > stats.(stat_index Mind)));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Nandin;
      evolution_tier = 3;
      photon_blast = Estlla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Mind)
                                      && stats.(stat_index Mind) >= stats.(stat_index Pow));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Naraka;
      evolution_tier = 3;
      photon_blast = Golla;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) > stats.(stat_index Dex)
                                      && stats.(stat_index Dex) >= stats.(stat_index Mind));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Ravana;
      evolution_tier = 3;
      photon_blast = Farlla;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Pow) > stats.(stat_index Mind)
                                      && stats.(stat_index Mind) > stats.(stat_index Dex));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Ribhava;
      evolution_tier = 3;
      photon_blast = Farlla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Pow)
                                      && stats.(stat_index Pow) > stats.(stat_index Mind));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Sita;
      evolution_tier = 3;
      photon_blast = Pilla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Mind)
                                      && stats.(stat_index Mind) >= stats.(stat_index Pow));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Soma;
      evolution_tier = 3;
      photon_blast = Estlla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) > stats.(stat_index Dex)
                                      && stats.(stat_index Dex) > stats.(stat_index Pow));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Fo;
    };
    {
      mag_type = Ushasu;
      evolution_tier = 3;
      photon_blast = Golla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) > stats.(stat_index Dex)
                                      && stats.(stat_index Dex) > stats.(stat_index Pow));
      section_id_condition = VSPRY;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Varaha;
      evolution_tier = 3;
      photon_blast = Golla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> (stats.(stat_index Pow) >= stats.(stat_index Dex)
                                      && stats.(stat_index Dex) >= stats.(stat_index Mind))
                                     || (stats.(stat_index Dex) == stats.(stat_index Mind)
                                         && stats.(stat_index Mind) > stats.(stat_index Pow)));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Varaha;
      evolution_tier = 3;
      photon_blast = Golla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Mind) > stats.(stat_index Pow)
                                      && stats.(stat_index Pow) >= stats.(stat_index Dex));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Varaha;
      evolution_tier = 3;
      photon_blast = Golla;
      feed_table = 4;
      mag_condition = None;
      stat_condition = (fun stats -> (stats.(stat_index Dex) >= stats.(stat_index Mind)
                                      && stats.(stat_index Mind) >= stats.(stat_index Pow))
                                     || (stats.(stat_index Pow) == stats.(stat_index Mind)
                                         && stats.(stat_index Mind) > stats.(stat_index Dex)));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Ra;
    };
    {
      mag_type = Yaksa;
      evolution_tier = 3;
      photon_blast = Golla;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Dex) > stats.(stat_index Mind)
                                      && stats.(stat_index Mind) >= stats.(stat_index Pow));
      section_id_condition = GBPOW;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Hu;
    };
    {
      mag_type = Bhima;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Mind)
                                     == stats.(stat_index Pow) + stats.(stat_index Dex));
      section_id_condition = GPO;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Fo;
    };
    {
      mag_type = Bhima;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Pow)
                                     == stats.(stat_index Dex) + stats.(stat_index Mind));
      section_id_condition = SPY;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Fo;
    };
    {
      mag_type = Deva;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Dex)
                                     == stats.(stat_index Pow) + stats.(stat_index Mind));
      section_id_condition = VBRW;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Hu;
    };
    {
      mag_type = Diwari;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Pow)
                                     == stats.(stat_index Dex) + stats.(stat_index Mind));
      section_id_condition = SPY;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Ra;
    };
    {
      mag_type = Nidra;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Dex)
                                     == stats.(stat_index Pow) + stats.(stat_index Mind));
      section_id_condition = VBRW;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Fo;
    };
    {
      mag_type = Nidra;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Mind)
                                     == stats.(stat_index Pow) + stats.(stat_index Dex));
      section_id_condition = GPO;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Fo;
    };
    {
      mag_type = Nidra;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Pow)
                                     == stats.(stat_index Dex) + stats.(stat_index Mind));
      section_id_condition = SPY;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Fo;
    };
    {
      mag_type = Pushan;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Dex)
                                     == stats.(stat_index Pow) + stats.(stat_index Mind));
      section_id_condition = VBRW;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Ra;
    };
    {
      mag_type = Pushan;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Mind)
                                     == stats.(stat_index Pow) + stats.(stat_index Dex));
      section_id_condition = GPO;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Ra;
    };
    {
      mag_type = Pushan;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Pow)
                                     == stats.(stat_index Dex) + stats.(stat_index Mind));
      section_id_condition = SPY;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Ra;
    };
    {
      mag_type = Rati;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Mind)
                                     == stats.(stat_index Pow) + stats.(stat_index Dex));
      section_id_condition = GPO;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Hu;
    };
    {
      mag_type = Rati;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 6;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Pow)
                                     == stats.(stat_index Dex) + stats.(stat_index Mind));
      section_id_condition = SPY;
      level_condition = 100;
      gender_condition = Male;
      class_condition = Hu;
    };
    {
      mag_type = Rukmin;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Dex)
                                     == stats.(stat_index Pow) + stats.(stat_index Mind));
      section_id_condition = VBRW;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Ra;
    };
    {
      mag_type = Rukmin;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Mind)
                                     == stats.(stat_index Pow) + stats.(stat_index Dex));
      section_id_condition = GPO;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Ra;
    };
    {
      mag_type = Sato;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 5;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Dex)
                                     == stats.(stat_index Pow) + stats.(stat_index Mind));
      section_id_condition = VBRW;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Fo;
    };
    {
      mag_type = Rukmin;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Dex)
                                     == stats.(stat_index Pow) + stats.(stat_index Mind));
      section_id_condition = VBRW;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Hu;
    };
    {
      mag_type = Rukmin;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Mind)
                                     == stats.(stat_index Pow) + stats.(stat_index Dex));
      section_id_condition = GPO;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Hu;
    };
    {
      mag_type = Rukmin;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = (fun stats -> stats.(stat_index Def) + stats.(stat_index Pow)
                                     == stats.(stat_index Dex) + stats.(stat_index Mind));
      section_id_condition = SPY;
      level_condition = 100;
      gender_condition = Female;
      class_condition = Hu;
    };
  |];;

let cell_evolutions = [|
    {
      mag_type = Agastya;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = no_stat_condition;
      section_id_condition = Any;
      level_condition = 50;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = AngelsWing;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = no_stat_condition;
      section_id_condition = Any;
      level_condition = 100;
      gender_condition = Any;
      class_condition = Any;
    };
    {
      mag_type = Chao;
      evolution_tier = 4;
      photon_blast = None;
      feed_table = 7;
      mag_condition = None;
      stat_condition = Array.for_all (fun stat -> stat > 35);
      section_id_condition = Any;
      level_condition = 0;
      gender_condition = Any;
      class_condition = Any;
    };
  |];;

let food_values (mag : mag) (food : food) = feed_tables.(mag.feed_table).(food_index food);;

let stat_can_level_up stat_index =
  let stat = stat_of_index stat_index in
  stat != Synchro && stat != Iq;;

let stat_level stat_value = stat_value / 100;;

let did_stat_level_up old_val new_val =
  (stat_level new_val) > (stat_level old_val);;

let cap_stat stat value =
  min (stat_max_value stat) value;;

let evolve_mag mag evo =
  if evo.photon_blast != None then (
    let empty_pb_slot = find_index mag.photon_blasts None in
    if empty_pb_slot > -1 then
      mag.photon_blasts.(empty_pb_slot) <- evo.photon_blast);
  {
    stats = mag.stats;
    level = mag.level;
    photon_blasts = mag.photon_blasts;
    feed_table = evo.feed_table;
    mag_type = evo.mag_type;
    evolution_tier = evo.evolution_tier;
  };;

let try_evolve_mag feeder mag =
  let num_evos = Array.length evolutions in
  let evo_condition evo =
    (* TODO: add conditions *)
    evo.level_condition >= mag.level
    && (if evo.gender_condition != Any then evo.gender_condition == feeder.gender else true)
    && (if evo.class_condition != Any then evo.class_condition == feeder.character_class else true) in
  let rec search i =
    if i >= num_evos then mag
    else (
      let evo = evolutions.(i) in
      if evo_condition evo then evolve_mag mag evo
      else (search (i + 1))) in
  search 0;;

let feed_mag feeder (mag : mag) (food : food) : mag =
  let food_values = food_values mag food in
  let num_level_ups = ref 0 in
  let increase_stat stat_index =
    let old_val = mag.stats.(stat_index) in
    let new_val = old_val + food_values.(stat_index) in
    if stat_can_level_up stat_index && did_stat_level_up old_val new_val then
      num_level_ups := !num_level_ups + 1;
    new_val in
  let level_up_order = Array.mapi (fun i _ -> i) mag.stats in
  Array.sort (fun a b -> mag.stats.(a) - mag.stats.(b)) level_up_order;
  let new_stats_unsorted = Array.map increase_stat level_up_order in
  let new_stats = Array.mapi (fun stat_index _ ->
                      let stat_val = new_stats_unsorted.(find_index level_up_order stat_index) in
                      let stat = stat_of_index stat_index in
                      cap_stat stat stat_val)
                             new_stats_unsorted in
  let mag_new_stats = {
    stats = new_stats;
    level = mag.level + !num_level_ups;
    photon_blasts = mag.photon_blasts;
    feed_table = mag.feed_table;
    mag_type = mag.mag_type;
    evolution_tier = mag.evolution_tier;
    } in
  if !num_level_ups > 1 then try_evolve_mag feeder mag_new_stats
  else mag_new_stats;;  

let default_mag () =
  let make_pbs (pb : photon_blast) =
    Array.make mag_photon_blast_max_amount pb in
  {
    stats = [|500; 0; 0; 0; 0; 0;|];
    level = 5;
    photon_blasts = make_pbs None;
    feed_table = 0;
    mag_type = Mag;
    evolution_tier = 0;
  };;

let random_mag () =
  let feeder = {
      section_id = Pinkal;
      gender = Male;
      character_class = Hu;
    } in
  let random_food () =
    food_of_index @@ Random.int @@ food_index Last in
  let rec feed_loop mag =
    if mag.level >= 200 then mag
    else feed_loop @@ feed_mag feeder mag @@ random_food () in
  feed_loop @@ default_mag ();;

let photon_blast_to_string = function
  | Twins -> "T"
  | Golla -> "G"
  | Pilla -> "P"
  | Estlla -> "E"
  | Leilla -> "L"
  | Farlla -> "F"
  | None -> "x";;

let mag_to_string (mag : mag) =
  let pb_names = Array.map photon_blast_to_string mag.photon_blasts in
  Printf.sprintf "%s Lv. %d [ DEF %d | POW %d | DEX %d | MIND %d ] [ SYNC %d | IQ %d ] [%s|%s|%s]\n"
                (mag_name mag.mag_type) mag.level
                (stat_level mag.stats.(stat_index Def))
                (stat_level mag.stats.(stat_index Pow))
                (stat_level mag.stats.(stat_index Dex))
                (stat_level mag.stats.(stat_index Mind))
                mag.stats.(stat_index Synchro)
                mag.stats.(stat_index Iq)
                pb_names.(0)
                pb_names.(1)
                pb_names.(2);;

let () =
  Printf.printf "%s\n" @@ mag_to_string (random_mag ());;
