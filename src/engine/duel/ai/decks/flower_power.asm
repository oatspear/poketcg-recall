AIActionTable_FlowerPower:
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
	dw KANGASKHAN
	dw EXEGGCUTE
	dw BULBASAUR
	dw NULL

.list_bench
	dw BULBASAUR
	dw EXEGGCUTE
	dw KANGASKHAN
	dw NULL

.list_retreat
	ai_retreat EXEGGCUTE, -2
	ai_retreat BULBASAUR, -2
	ai_retreat IVYSAUR,   -1
	dw NULL

.list_energy
	ai_energy BULBASAUR,      2, +1
	ai_energy IVYSAUR,        3, +0
	ai_energy VENUSAUR_LV67,  4, +0
	ai_energy EXEGGCUTE,      2, +1
	ai_energy EXEGGUTOR,     22, +0
	ai_energy KANGASKHAN,     4, -1
	dw NULL

.list_prize
	dw VENUSAUR_LV67
	dw KANGASKHAN
	dw NULL

.store_list_pointers
	store_list_pointer wAICardListAvoidPrize, .list_prize
	store_list_pointer wAICardListArenaPriority, .list_arena
	store_list_pointer wAICardListBenchPriority, .list_bench
	store_list_pointer wAICardListPlayFromHandPriority, .list_bench
	store_list_pointer wAICardListRetreatBonus, .list_retreat
	store_list_pointer wAICardListEnergyBonus, .list_energy
	ret
