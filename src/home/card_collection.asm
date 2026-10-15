; return, in hl, the total amount of cards owned anywhere, including duplicates
GetAmountOfCardsOwned::
	push de
	push bc
	call EnableSRAM
	ld hl, 0
; remove code: use cards in multiple decks
;	ld de, sDeck1Cards
;	ld c, NUM_DECKS
;.next_deck
;	push bc
;	ld a, [de]
;	inc de
;	ld c, a
;	ld a, [de]
;	dec de
;	or c
;	pop bc
;	jr z, .skip_deck ; jump if deck empty
;	ld a, c
;	ld bc, DECK_SIZE
;	add hl, bc
;	ld c, a
;.skip_deck
;	ld a, sDeck2Cards - sDeck1Cards
;	add e
;	ld e, a
;	ld a, $0
;	adc d
;	ld d, a ; de = sDeck*Cards[x]
;	dec c
;	jr nz, .next_deck

	; hl = DECK_SIZE * (no. of non-empty decks)
	ld de, sCardCollection
	ld bc, CARD_COLLECTION_SIZE
.next_card
	ld a, [de]
	bit CARD_NOT_OWNED_F, a
	jr nz, .skip_card
	push bc
	ld c, a ; card count in sCardCollection
	ld b, $0
	add hl, bc
	pop bc
.skip_card
	inc de
	dec bc
	ld a, b
	or c
	jr nz, .next_card
	call DisableSRAM
	pop bc
	pop de
	ret

; return carry if the count in sCardCollection of the card with id given in de is 0.
; also return the count (amount owned outside of decks) in a.
GetCardCountInCollection::
	push hl
	call EnableSRAM
	ld hl, sCardCollection
	add hl, de
	ld a, [hl]
	call DisableSRAM
	pop hl
	and CARD_COUNT_MASK
	ret nz
	scf
	ret

; creates a list at wTempCardCollection of every card the player owns and how many
CreateTempCardCollection::
	call EnableSRAM
	ld hl, sCardCollection
	ld de, wTempCardCollection
	ld bc, CARD_COLLECTION_SIZE
	call CopyDataHLtoDE
	jp DisableSRAM

; add card with id given in de to sCardCollection, provided that
; the player has less than MAX_AMOUNT_OF_CARD (99) of them
AddCardToCollection::
	push hl
	push de
	push bc
	push de
	call CreateTempCardCollection
	pop de
	call EnableSRAM
	ld hl, wTempCardCollection
	add hl, de
	ld a, [hl]
	and CARD_COUNT_MASK
	cp MAX_AMOUNT_OF_CARD
	jr nc, .already_max
	ld hl, sCardCollection
	add hl, de
	ld a, [hl]
	and CARD_COUNT_MASK
	inc a
	ld [hl], a
.already_max
	call DisableSRAM
	pop bc
	pop de
	pop hl
	ret

; remove a card with id given in de from sCardCollection (decrement its count if non-0)
RemoveCardFromCollection::
	push hl
	call EnableSRAM
	ld hl, sCardCollection
	add hl, de
	ld a, [hl]
	and CARD_COUNT_MASK
	jr z, .zero
	dec a
	ld [hl], a
.zero
	call DisableSRAM
	pop hl
	ret

; return the amount of different cards that the player has collected in de
; return NUM_CARDS in bc
GetCardAlbumProgress::
	push hl
	call EnableSRAM
	ld de, 0
	ld hl, sCardCollection
.next_card
	bit CARD_NOT_OWNED_F, [hl]
	jr nz, .skip
	inc de ; if this card owned
.skip
	inc hl
	ld a, l
	cp LOW(sCardCollection + CARD_COLLECTION_SIZE)
	jr nz, .next_card
	ld a, h
	cp HIGH(sCardCollection + CARD_COLLECTION_SIZE)
	jr nz, .next_card

	call DisableSRAM
	pop hl
	ret
