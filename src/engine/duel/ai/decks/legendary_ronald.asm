AIActionTable_LegendaryRonald:
	dw .do_turn ; unused
	dw .do_turn
	dw .start_duel
	dw .forced_switch
	dw .ko_switch
	dw .take_prize

.do_turn
	jp AIDoTurn_LegendaryRonald

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
	dw LICKITUNG
	dw SCYTHER
	dw MEWTWO_LV60
	dw GRIMER
	dw NULL

.list_bench
	dw MEWTWO_LV60
	dw SCYTHER
	dw LICKITUNG
	dw GRIMER
	dw NULL

.list_retreat
	ai_retreat GRIMER, -2
	dw NULL

.list_energy
	ai_energy GRIMER,      1, +0
	ai_energy MUK,         3, -2
	ai_energy SCYTHER,     3, +0
	ai_energy MEWTWO_LV60, 3, +2
	ai_energy LICKITUNG,   1, +0
	dw NULL

.list_prize
	dw MUK
	dw MEWTWO_LV60
	dw GAMBLER
	dw NULL

.store_list_pointers
	store_list_pointer wAICardListAvoidPrize, .list_prize
	store_list_pointer wAICardListArenaPriority, .list_arena
	store_list_pointer wAICardListBenchPriority, .list_bench
	store_list_pointer wAICardListPlayFromHandPriority, .list_bench
	store_list_pointer wAICardListRetreatBonus, .list_retreat
	store_list_pointer wAICardListEnergyBonus, .list_energy
	ret

AIDoTurn_LegendaryRonald:
	jp AIMainTurnLogic
