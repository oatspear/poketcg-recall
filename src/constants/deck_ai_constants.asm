; wPreviousAIFlags and wCurrentAIFlags constants
DEF AI_FLAG_USED_PLUSPOWER     EQU 1 << 0
DEF AI_FLAG_USED_SWITCH        EQU 1 << 1
DEF AI_FLAG_USED_PROFESSOR_OAK EQU 1 << 2
DEF AI_FLAG_MODIFIED_HAND      EQU 1 << 3
DEF AI_FLAG_USED_GUST_OF_WIND  EQU 1 << 4

; used as input for AIProcessEnergyCards to determine what to check
; and whether to play card after the routine is over.
; I suspect AI_ENERGY_FLAG_DONT_PLAY to be a flag to signal the routine
; not to actually play the energy card after it's finished,
; but AIProcessEnergyCards checks whether ANY flag is set in order
; to decide not to play it, so it's redundant in the presence of another flag.
DEF AI_ENERGY_FLAG_DONT_PLAY       EQU 1 << 0 ; whether to play energy card (?)
DEF AI_ENERGY_FLAG_SKIP_EVOLUTION  EQU 1 << 1 ; whether to check if card has evolutions
DEF AI_ENERGY_FLAG_SKIP_ARENA_CARD EQU 1 << 7 ; whether to include Arena card in determining which card to attach energy

; Used to determine which Trainer cards for AI to process in AIProcessHandTrainerCards.
; These go in chronological order.
	const_def 1

	const AI_TRAINER_CARD_PHASE_01 ; $1
	; Imakuni?
	; Gambler

	const AI_TRAINER_CARD_PHASE_02 ; $2
	; Maintenance
	; Poké Ball
	; Computer Search
	; Pokémon Trader
	; Energy Search

	const AI_TRAINER_CARD_PHASE_03 ; $3
	; Pokédex
	; Recycle

	const AI_TRAINER_CARD_PHASE_04 ; $4
	; Bill

	const AI_TRAINER_CARD_PHASE_05 ; $5
	; Revive
	; Clefairy Doll
	; Mysterious Fossil

	const AI_TRAINER_CARD_PHASE_06 ; $6
	; Scoop Up

	const AI_TRAINER_CARD_PHASE_07 ; $7
	; Pokémon Breeder

	const AI_TRAINER_CARD_PHASE_08 ; $8
	; Mr. Fuji

	const AI_TRAINER_CARD_PHASE_09 ; $9
	; (Super) Potion
	; Full Heal

	const AI_TRAINER_CARD_PHASE_10 ; $a
	; Pokémon Center

	const AI_TRAINER_CARD_PHASE_11 ; $b
	; (Super) Potion

	const AI_TRAINER_CARD_PHASE_12 ; $c
	; Switch

	const AI_TRAINER_CARD_PHASE_13 ; $d
	; (Super) Energy Retrieval

	const AI_TRAINER_CARD_PHASE_14 ; $e
	; (Super) Energy Removal
	; Imposter Professor Oak
	; Pokémon Flute

	const AI_TRAINER_CARD_PHASE_15 ; $f
	; Item Finder

	const AI_TRAINER_CARD_PHASE_16 ; $f
	; Gust of Wind

	const AI_TRAINER_CARD_PHASE_17 ; $f
	; Defender
	; Pluspower

	const AI_TRAINER_CARD_PHASE_18 ; $f
	; Lass

	const AI_TRAINER_CARD_PHASE_19 ; $f
	; Professor Oak

; used by wAIBarrierFlagCounter to determine
; whether Player is running MewtwoLv53 mill deck.
; flag set means true, flag not set means false.
DEF AI_MEWTWO_MILL_F EQU 7
DEF AI_MEWTWO_MILL   EQU 1 << AI_MEWTWO_MILL_F

; defines the behaviour of HandleAIEnergyTrans, for determining
; whether to move energy cards from the Bench to the Arena or vice-versa
; and the number of energy cards needed for achieving that.
DEF AI_ENERGY_TRANS_RETREAT  EQU $9 ; moves energy cards needed for Retreat Cost
DEF AI_ENERGY_TRANS_ATTACK   EQU $d ; moves energy cards needed for second attack
DEF AI_ENERGY_TRANS_TO_BENCH EQU $e ; moves energy cards away from Arena card

; used to know which AI routine to call in
; the AIAction pointer tables in AIDoAction
	const_def 1
	const AIACTION_DO_TURN       ; $1
	const AIACTION_START_DUEL    ; $2
	const AIACTION_FORCED_SWITCH ; $3
	const AIACTION_KO_SWITCH     ; $4
	const AIACTION_TAKE_PRIZE    ; $5

; this bit is set when the AI decides to use Peek on their Prize cards,
; with the following bits deciding which one to Peek. That is:
;	%10'0000 = first prize card
;	%10'0001 = second prize card
;	%10'0010 = third prize card
;	etc...
DEF AI_PEEK_TARGET_PRIZE_F EQU 6
DEF AI_PEEK_TARGET_PRIZE   EQU 1 << AI_PEEK_TARGET_PRIZE_F
; this bit is set when the AI decides to use Peek on Player hand card,
; with the following bits deciding which one to Peek. That is:
;	%1XXX XXXX, where XXX XXXX is the deck index of card chosen
DEF AI_PEEK_TARGET_HAND_F EQU 7
DEF AI_PEEK_TARGET_HAND   EQU 1 << AI_PEEK_TARGET_HAND_F
; all bits set means AI chose to look at Player's top deck card
DEF AI_PEEK_TARGET_DECK EQU $ff
