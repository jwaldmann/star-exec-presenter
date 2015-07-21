	catch (not generate) exceptions in sendRequestRaw,
	increase timeout

	remove getConnection. It's in the session state,
	and nobody outside Presenter.StarExec.Connection
	needs this.

	
	
