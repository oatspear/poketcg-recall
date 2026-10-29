AIActionTable_LegendaryDragonite:
	dw .do_turn ; unused
	dw .do_turn
	dw .start_duel
	dw .forced_switch
	dw .ko_switch
	dw .take_prize

.do_turn
	jp AIDoTurn_LegendaryDragonite

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
	dw CHARMANDER
	dw DRATINI
	dw MAGIKARP
	dw NULL

.list_bench
	dw DRATINI
	dw MAGIKARP
	dw CHARMANDER
	dw NULL

.list_retreat
	ai_retreat CHARMANDER, -1
	ai_retreat MAGIKARP,   -5
	dw NULL

.list_energy
	ai_energy CHARMANDER,     2, +1
	ai_energy CHARMELEON,     3, +1
	ai_energy CHARIZARD,      5, +0
	ai_energy MAGIKARP,       3, +1
	ai_energy GYARADOS,       4, -1
	ai_energy DRATINI,        2, +0
	ai_energy DRAGONAIR,      3, +0
	ai_energy DRAGONITE_LV41, 3, -1
	dw NULL

.list_prize
	dw GAMBLER
	dw DRAGONITE_LV41
	dw NULL

.store_list_pointers
	store_list_pointer wAICardListAvoidPrize, .list_prize
	store_list_pointer wAICardListArenaPriority, .list_arena
	store_list_pointer wAICardListBenchPriority, .list_bench
	store_list_pointer wAICardListPlayFromHandPriority, .list_bench
	store_list_pointer wAICardListRetreatBonus, .list_retreat
	store_list_pointer wAICardListEnergyBonus, .list_energy
	ret

AIDoTurn_LegendaryDragonite:
	jp AIMainTurnLogic


; if Arena card is Kangaskhan and doesn't
; have Energy cards attached, raise score.
; otherwise run normal AI energy attach routine.
ScoreLegendaryDragoniteCardsForEnergyAttachment:
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
	cp16 KANGASKHAN
	ret nz

	ld e, PLAY_AREA_ARENA
	call CountNumberOfEnergyCardsAttached
	or a
	ret nz  ; already has Energy attached

	ld a, [wPlayAreaEnergyAIScore + PLAY_AREA_ARENA]
	add 5
	ld [wPlayAreaEnergyAIScore + PLAY_AREA_ARENA], a
	ret
