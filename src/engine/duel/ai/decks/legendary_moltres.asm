AIActionTable_LegendaryMoltres:
	dw .do_turn ; unused
	dw .do_turn
	dw .start_duel
	dw .forced_switch
	dw .ko_switch
	dw .take_prize

.do_turn
	jp AIDoTurn_LegendaryMoltres

.start_duel
	call InitAIDuelVars
	call .store_list_pointers
	call SetUpBossStartingHandAndDeck
	call TrySetUpBossStartingPlayArea
	ret nc ; Play Area set up was successful
	jp AIPlayInitialBasicCards

.forced_switch
	jp AIDecideBenchPokemonToSwitchTo

.ko_switch
	jp AIDecideBenchPokemonToSwitchTo

.take_prize
	jp AIPickPrizeCards

.list_arena
	dw MAGMAR_LV31
	dw GROWLITHE
	dw VULPIX
	dw MOLTRES_LV35
	dw MOLTRES_LV37
	dw NULL

.list_play_hand
	dw MOLTRES_LV37
	; fallthrough

.list_bench
	dw MOLTRES_LV35
	dw VULPIX
	dw GROWLITHE
	dw MAGMAR_LV31
	dw NULL

.list_retreat
	ai_retreat GROWLITHE, -5
	ai_retreat VULPIX,    -5
	dw NULL

.list_energy
	ai_energy VULPIX,         3, +0
	ai_energy NINETALES_LV35, 3, +1
	ai_energy GROWLITHE,      3, +1
	ai_energy ARCANINE_LV45,  4, +1
	ai_energy MAGMAR_LV31,    2, +0
	ai_energy MOLTRES_LV37,   3, +2
	ai_energy MOLTRES_LV35,   4, +2
	dw NULL

.list_prize
	dw ENERGY_REMOVAL
	dw MOLTRES_LV37
	dw NULL

.store_list_pointers
	store_list_pointer wAICardListAvoidPrize, .list_prize
	store_list_pointer wAICardListArenaPriority, .list_arena
	store_list_pointer wAICardListBenchPriority, .list_bench
	store_list_pointer wAICardListPlayFromHandPriority, .list_play_hand
	store_list_pointer wAICardListRetreatBonus, .list_retreat
	store_list_pointer wAICardListEnergyBonus, .list_energy
	ret

AIDoTurn_LegendaryMoltres:
	jp AIMainTurnLogic


; if Magmar Lv31 is the Arena card and has no energy attached, raise score.
ScoreLegendaryMoltresCardsForEnergyAttachment:
	ld a, DUELVARS_ARENA_CARD
	call GetTurnDuelistVariable
	call GetCardIDFromDeckIndex
	cp16 MAGMAR_LV31
	ret nz

; Magmar Lv31 is the Arena card
	ld e, PLAY_AREA_ARENA
	call CountNumberOfEnergyCardsAttached
	or a
	ret nz

	ld a, [wPlayAreaEnergyAIScore + PLAY_AREA_ARENA]
	add 5
	ld [wPlayAreaEnergyAIScore + PLAY_AREA_ARENA], a
	ret
