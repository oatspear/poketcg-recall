DEF AI_DEBUG_PRINTS EQU FALSE

; AI logic used by general decks
AIActionTable_GeneralDecks:
	dw .do_turn ; unused
	dw .do_turn
	dw .start_duel
	dw .forced_switch
	dw .ko_switch
	dw .take_prize

.do_turn
	jp AIMainTurnLogic

.start_duel
	call InitAIDuelVars
	jp AIPlayInitialBasicCards

.forced_switch
	jp AIDecideBenchPokemonToSwitchTo

.ko_switch
	jp AIDecideBenchPokemonToSwitchTo

.take_prize:
	jp AIPickPrizeCards


; handle AI routines for a whole turn
AIMainTurnLogic:
; initialize variables
	call InitAITurnVars

; Phase 1: Anti-mill
; Handle the special anti-mill strategy.
.phase_1
IF AI_DEBUG_PRINTS
	ldtx hl, BulbasaurName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_01
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, IvysaurName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIAntiMewtwoDeckStrategy
	jp nc, .try_attack

; Phase 2: Deck Search
; Handle cards that search the deck without any cost.
.phase_2

IF AI_DEBUG_PRINTS
	ldtx hl, SquirtleName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_02
	call AIProcessHandTrainerCards
	jr c, .phase_2

; Phase 3: Deck Sorting
; Handle cards that sort or change the top of the deck.

IF AI_DEBUG_PRINTS
	ldtx hl, WartortleName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_03
	call AIProcessHandTrainerCards

; Phase 4: Unconditional Card Draw

IF AI_DEBUG_PRINTS
	ldtx hl, BlastoiseName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_04
	call AIProcessHandTrainerCards
	jr c, .phase_1

; Phase 5: Scoop Up

IF AI_DEBUG_PRINTS
	ldtx hl, CharizardName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAICowardice

IF AI_DEBUG_PRINTS
	ldtx hl, ButterfreeName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_05
	call AIProcessHandTrainerCards

; Phase 6: Bench Filling Trainers
; Handle cards that have priority over Pokémon.

IF AI_DEBUG_PRINTS
	ldtx hl, MetapodName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_06
	call AIProcessHandTrainerCards

; Breakpoint: Play Pokémon Cards

IF AI_DEBUG_PRINTS
	ldtx hl, CaterpieName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIDecidePlayPokemonCard

; Phase 7: Pokémon Breeder

IF AI_DEBUG_PRINTS
	ldtx hl, WeedleName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_07
	call AIProcessHandTrainerCards

; Breakpoint: evaluate retreat

IF AI_DEBUG_PRINTS
	ldtx hl, BeedrillName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIProcessRetreat

; Phase 8: Mr. Fuji

IF AI_DEBUG_PRINTS
	ldtx hl, KakunaName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_08
	call AIProcessHandTrainerCards

; Phase 9: Healing

IF AI_DEBUG_PRINTS
	ldtx hl, CharmanderName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIDamageSwap

IF AI_DEBUG_PRINTS
	ldtx hl, CharmeleonName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIPkmnPowers
	ret c ; return if turn ended

IF AI_DEBUG_PRINTS
	ldtx hl, PidgeyName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_09
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, NidokingName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_10
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, NidoranFName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_11
	call AIProcessHandTrainerCards

; Breakpoint: evaluate retreat

IF AI_DEBUG_PRINTS
	ldtx hl, BeedrillName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIProcessRetreat

; Phase 13: Energy Retrieval

IF AI_DEBUG_PRINTS
	ldtx hl, PidgeotName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_13
	call AIProcessHandTrainerCards

; Breakpoint: Energy attachment

IF AI_DEBUG_PRINTS
	ldtx hl, VenusaurName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIGoGoRainDanceEnergy

IF AI_DEBUG_PRINTS
	ldtx hl, RattataName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIProcessAndTryToPlayEnergy

; Phase 14: Disruption

IF AI_DEBUG_PRINTS
	ldtx hl, PikachuName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_14
	call AIProcessHandTrainerCards

; Phase 15: Item Finder

IF AI_DEBUG_PRINTS
	ldtx hl, RaichuName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_15
	call AIProcessHandTrainerCards
	jr c, .phase_1

; Phase 16: Gusting

IF AI_DEBUG_PRINTS
	ldtx hl, ClefableName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_16
	call AIProcessHandTrainerCards

; Breakpoint: Energy Trans

IF AI_DEBUG_PRINTS
	ldtx hl, SandslashName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_ENERGY_TRANS_ATTACK
	farcall HandleAIEnergyTrans

; Phase 17: Tools
; Handle Pluspower and Defender

IF AI_DEBUG_PRINTS
	ldtx hl, VulpixName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_17
	call AIProcessHandTrainerCards

; Phase 18: Professor Oak

IF AI_DEBUG_PRINTS
	ldtx hl, EkansName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_18
	call AIProcessHandTrainerCards

; if used Professor Oak, process new hand
; if not, then proceed to attack.

IF AI_DEBUG_PRINTS
	ldtx hl, ClefairyName
	call DrawWideTextBox_WaitForInput
ENDC

	jp c, .phase_1

	; ld a, [wPreviousAIFlags]
	; and AI_FLAG_USED_PROFESSOR_OAK
	; ; make sure that flags are reset to avoid infinite loops
	; push af
	; ld a, [wPreviousAIFlags]
	; and AI_FLAG_USED_GUST_OF_WIND  ; flags to keep
	; ld [wPreviousAIFlags], a
	; pop af
	; jp nz, .phase_1

; Phase 19: Lass

IF AI_DEBUG_PRINTS
	ldtx hl, NinetalesName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_19
	call AIProcessHandTrainerCards

.try_attack

IF AI_DEBUG_PRINTS
	ldtx hl, VenomothName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_ENERGY_TRANS_TO_BENCH
	farcall HandleAIEnergyTrans

; attack if possible, if not,
; finish turn without attacking.

IF AI_DEBUG_PRINTS
	ldtx hl, TaurosName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIProcessAndTryToUseAttack
	ret c ; return if AI attacked

IF AI_DEBUG_PRINTS
	ldtx hl, ChanseyName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, OPPACTION_FINISH_NO_ATTACK
	bank1call AIMakeDecision

IF AI_DEBUG_PRINTS
	ldtx hl, DittoName
	call DrawWideTextBox_WaitForInput
ENDC
	ret


; handles AI retreating logic
AIProcessRetreat:
	ld a, [wAIRetreatedThisTurn]
	or a
	ret nz ; return, already retreated this turn

IF AI_DEBUG_PRINTS
	ldtx hl, AbraName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIDecideWhetherToRetreat
	ret nc ; return if not retreating

IF AI_DEBUG_PRINTS
	ldtx hl, KadabraName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIDecideBenchPokemonToSwitchTo
	ret c ; return if no Bench Pokemon

IF AI_DEBUG_PRINTS
	push af
	ldtx hl, AlakazamName
	call DrawWideTextBox_WaitForInput
	pop af
ENDC

; store Play Area to retreat to and
; set wAIRetreatedThisTurn to true
	ld [wAIPlayAreaCardToSwitch], a
	ld a, TRUE
	ld [wAIRetreatedThisTurn], a

	xor a ; PLAY_AREA_ARENA
	ldh [hTempPlayAreaLocation_ff9d], a
	call GetPlayAreaCardRetreatCost
	or a
	jr z, .retreat_free ; free retreat

; if AI can use Switch from hand, use it instead...
	ld a, AI_TRAINER_CARD_PHASE_12
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, SlowpokeName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, [wPreviousAIFlags]
	and AI_FLAG_USED_SWITCH
	jr nz, .used_switch

; ... else try retreating normally.
.retreat_free

IF AI_DEBUG_PRINTS
	ldtx hl, SlowbroName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, [wAIPlayAreaCardToSwitch]
	jp AITryToRetreat

.used_switch
; if AI used switch, unset its AI flag
	ld a, [wPreviousAIFlags]
	and ~AI_FLAG_USED_SWITCH ; clear Switch flag
	ld [wPreviousAIFlags], a

IF AI_DEBUG_PRINTS
	ldtx hl, VoltorbName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_ENERGY_TRANS_RETREAT
	farcall HandleAIEnergyTrans

IF AI_DEBUG_PRINTS
	ldtx hl, ElectrodeName
	call DrawWideTextBox_WaitForInput
ENDC

	ret
