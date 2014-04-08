bot: bot.hs
	@ghc -O2 bot.hs

serve: bot
	@./bot
