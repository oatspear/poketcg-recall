AIActionTable_LegendaryZapdos:
	dw .do_turn ; unused
	dw .do_turn
	dw .start_duel
	dw .forced_switch
	dw .ko_switch
	dw .take_prize

.do_turn
	jp AIDoTurn_LegendaryZapdos

.start_duel
	call InitAIDuelVars
	call .store_list_pointers
	call SetUpBossStartingHandAndDeck
	call TrySetUpBossStartingPlayArea
	ret nc
	jp AIPlayInitialBasicCards

.forced_switch
	jp AIDecideBenchPokemonToSwitchTo

.ko_switch
	jp AIDecideBenchPokemonToSwitchTo

.take_prize
	jp AIPickPrizeCards

.list_arena
	dw ELECTABUZZ_LV35
	dw VOLTORB
	dw EEVEE
	dw ZAPDOS_LV40
	dw ZAPDOS_LV64
	dw ZAPDOS_LV68
	dw NULL

.list_bench
	dw ZAPDOS_LV64
	dw ZAPDOS_LV40
	dw EEVEE
	dw VOLTORB
	dw ELECTABUZZ_LV35
	dw NULL

.list_retreat
	ai_retreat EEVEE,           -5
	ai_retreat VOLTORB,         -5
	ai_retreat ELECTABUZZ_LV35, -5
	dw NULL

.list_energy
	ai_energy VOLTORB,         1, -1
	ai_energy ELECTRODE_LV35,  3, +0
	ai_energy ELECTABUZZ_LV35, 2, -1
	ai_energy JOLTEON_LV29,    3, +1
	ai_energy ZAPDOS_LV40,     4, +2
	ai_energy ZAPDOS_LV64,     4, +2
	ai_energy ZAPDOS_LV68,     3, +1
	ai_energy EEVEE,           3, +0
	dw NULL

.list_prize
	dw GAMBLER
	dw ZAPDOS_LV68
	dw NULL

.store_list_pointers
	store_list_pointer wAICardListAvoidPrize, .list_prize
	store_list_pointer wAICardListArenaPriority, .list_arena
	store_list_pointer wAICardListBenchPriority, .list_bench
	store_list_pointer wAICardListPlayFromHandPriority, .list_bench
	store_list_pointer wAICardListRetreatBonus, .list_retreat
	store_list_pointer wAICardListEnergyBonus, .list_energy
	ret

AIDoTurn_LegendaryZapdos:
; initialize variables
	call InitAITurnVars
	farcall HandleAIAntiMewtwoDeckStrategy
	jp nc, .try_attack
; process Trainer cards
	ld a, AI_TRAINER_CARD_PHASE_01
	call AIProcessHandTrainerCards
	ld a, AI_TRAINER_CARD_PHASE_04
	call AIProcessHandTrainerCards
; play Pokemon from hand
	call AIDecidePlayPokemonCard
	ret c ; return if turn ended
	ld a, AI_TRAINER_CARD_PHASE_07
	call AIProcessHandTrainerCards
	call AIProcessRetreat
	ld a, AI_TRAINER_CARD_PHASE_10
	call AIProcessHandTrainerCards
; play Energy card if possible.
	call AIProcessAndTryToPlayEnergy
; play Pokemon from hand again
	call AIDecidePlayPokemonCard
	ret c ; return if turn ended
	ld a, AI_TRAINER_CARD_PHASE_13
	call AIProcessHandTrainerCards
.try_attack
; attack if possible, if not,
; finish turn without attacking.
	call AIProcessAndTryToUseAttack
	ret c ; return if turn ended
	ld a, OPPACTION_FINISH_NO_ATTACK
	bank1call AIMakeDecision
	ret


; if Arena card is Voltorb and there's Electrode Lv35 in hand,
; or if it's Electabuzz, raise score if it doesn't have any energy attached.
ScoreLegendaryZapdosCardsForEnergyAttachment:
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
	cp16 VOLTORB
	jr nz, .check_electabuzz
	ld de, ELECTRODE_LV35
	call LookForCardIDInHandList_Bank5
	jr c, .voltorb_or_electabuzz
	ret

.check_electabuzz
	cp16 ELECTABUZZ_LV35
	ret nz

.voltorb_or_electabuzz
	ld e, PLAY_AREA_ARENA
	call CountNumberOfEnergyCardsAttached
	or a
	ret nz

	ld a, [wPlayAreaEnergyAIScore + PLAY_AREA_ARENA]
	add 5
	ld [wPlayAreaEnergyAIScore + PLAY_AREA_ARENA], a
	ret
