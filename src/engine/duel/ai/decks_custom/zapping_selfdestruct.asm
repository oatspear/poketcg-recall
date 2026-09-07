AIActionTable_ZappingSelfdestruct:
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
	dw ELECTABUZZ_LV35
	dw MAGNEMITE_LV13
	dw SPEAROW
	dw NULL

.list_bench
	dw MAGNEMITE_LV13
	dw ELECTABUZZ_LV35
	dw SPEAROW
	dw NULL

.list_retreat
	ai_retreat MAGNEMITE_LV13, -1
	dw NULL

.list_energy
	ai_energy MAGNEMITE_LV13,  2, +1
	ai_energy MAGNETON_LV28,   4, +0
	ai_energy ELECTABUZZ_LV35, 2, +1
	ai_energy SPEAROW,         1, +0
	ai_energy FEAROW,          4, +0
	dw NULL

.list_prize
	dw ELECTABUZZ_LV35
	dw NULL

.store_list_pointers
	store_list_pointer wAICardListAvoidPrize, .list_prize
	store_list_pointer wAICardListArenaPriority, .list_arena
	store_list_pointer wAICardListBenchPriority, .list_bench
	store_list_pointer wAICardListPlayFromHandPriority, .list_bench
	store_list_pointer wAICardListRetreatBonus, .list_retreat
	store_list_pointer wAICardListEnergyBonus, .list_energy
	ret
