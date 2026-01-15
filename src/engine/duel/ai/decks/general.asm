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

; handle Pkmn Powers

IF AI_DEBUG_PRINTS
	ldtx hl, VenusaurName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIGoGoRainDanceEnergy

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
	ldtx hl, CharizardName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAICowardice

; process Trainer cards phase 2 through 4

IF AI_DEBUG_PRINTS
	ldtx hl, SquirtleName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_02
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, WartortleName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_03
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, BlastoiseName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_04
	call AIProcessHandTrainerCards

; play Pokemon from hand

IF AI_DEBUG_PRINTS
	ldtx hl, CaterpieName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIDecidePlayPokemonCard
	ret c ; return if turn ended

; process Trainer cards phase 5 through 12

IF AI_DEBUG_PRINTS
	ldtx hl, MetapodName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_05
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, ButterfreeName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_06
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, WeedleName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_07
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, KakunaName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_08
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, BeedrillName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIProcessRetreat

IF AI_DEBUG_PRINTS
	ldtx hl, PidgeyName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_10
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, PidgeottoName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_11
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, PidgeotName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_12
	call AIProcessHandTrainerCards

; play Energy card if possible

IF AI_DEBUG_PRINTS
	ldtx hl, RattataName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIProcessAndTryToPlayEnergy

; play Pokemon from hand again

IF AI_DEBUG_PRINTS
	ldtx hl, RaticateName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIDecidePlayPokemonCard

; handle Pkmn Powers again

IF AI_DEBUG_PRINTS
	ldtx hl, SpearowName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIDamageSwap

IF AI_DEBUG_PRINTS
	ldtx hl, FearowName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIPkmnPowers
	ret c ; return if turn ended

IF AI_DEBUG_PRINTS
	ldtx hl, SandshrewName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIGoGoRainDanceEnergy

IF AI_DEBUG_PRINTS
	ldtx hl, SandslashName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_ENERGY_TRANS_ATTACK
	farcall HandleAIEnergyTrans

; process Trainer cards phases 13 and 15

IF AI_DEBUG_PRINTS
	ldtx hl, PikachuName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_13
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, RaichuName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_15
	call AIProcessHandTrainerCards

; if used Professor Oak, process new hand
; if not, then proceed to attack.

IF AI_DEBUG_PRINTS
	ldtx hl, ClefairyName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, [wPreviousAIFlags]
	and AI_FLAG_USED_PROFESSOR_OAK

IF AI_DEBUG_PRINTS
	jp z, .try_attack
ELSE
	jr z, .try_attack
ENDC

IF AI_DEBUG_PRINTS
	ldtx hl, ClefableName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_01
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, VulpixName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_02
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, NinetalesName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_03
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, EkansName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_04
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, ArbokName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIDecidePlayPokemonCard
	ret c ; return if turn ended

IF AI_DEBUG_PRINTS
	ldtx hl, NidoranMName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_05
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, NidorinoName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_06
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, NidokingName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_07
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, NidoranFName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_08
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, NidorinaName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIProcessRetreat

IF AI_DEBUG_PRINTS
	ldtx hl, NidoqueenName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_10
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, GrowlitheName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_11
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, ArcanineName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_12
	call AIProcessHandTrainerCards

IF AI_DEBUG_PRINTS
	ldtx hl, OddishName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIProcessAndTryToPlayEnergy

IF AI_DEBUG_PRINTS
	ldtx hl, GloomName
	call DrawWideTextBox_WaitForInput
ENDC

	call AIDecidePlayPokemonCard

IF AI_DEBUG_PRINTS
	ldtx hl, VileplumeName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIDamageSwap

IF AI_DEBUG_PRINTS
	ldtx hl, BellsproutName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIPkmnPowers
	ret c ; return if turn ended

IF AI_DEBUG_PRINTS
	ldtx hl, WeepinbellName
	call DrawWideTextBox_WaitForInput
ENDC

	farcall HandleAIGoGoRainDanceEnergy

IF AI_DEBUG_PRINTS
	ldtx hl, VictreebelName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_ENERGY_TRANS_ATTACK
	farcall HandleAIEnergyTrans

IF AI_DEBUG_PRINTS
	ldtx hl, VenonatName
	call DrawWideTextBox_WaitForInput
ENDC

	ld a, AI_TRAINER_CARD_PHASE_13
	call AIProcessHandTrainerCards
	; skip AI_TRAINER_CARD_PHASE_15

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
	ld a, AI_TRAINER_CARD_PHASE_09
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
