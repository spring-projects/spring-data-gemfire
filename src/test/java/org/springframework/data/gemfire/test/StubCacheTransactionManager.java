/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import com.gemstone.gemfire.cache.CacheTransactionManager;
import com.gemstone.gemfire.cache.CommitConflictException;
import com.gemstone.gemfire.cache.TransactionId;
import com.gemstone.gemfire.cache.TransactionListener;
import com.gemstone.gemfire.cache.TransactionWriter;


/**
 * @author David Turanski
 *
 */
public class StubCacheTransactionManager implements CacheTransactionManager {

	private List<TransactionListener> listeners = new ArrayList<TransactionListener>();

	private TransactionWriter writer;

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#begin()
	 */
	@Override
	public void begin() {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#commit()
	 */
	@Override
	public void commit() throws CommitConflictException {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#rollback()
	 */
	@Override
	public void rollback() {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#suspend()
	 */
	@Override
	public TransactionId suspend() {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#resume(com.gemstone.gemfire.cache.TransactionId)
	 */
	@Override
	public void resume(TransactionId transactionId) {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#isSuspended(com.gemstone.gemfire.cache.TransactionId)
	 */
	@Override
	public boolean isSuspended(TransactionId transactionId) {
		return false;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#tryResume(com.gemstone.gemfire.cache.TransactionId)
	 */
	@Override
	public boolean tryResume(TransactionId transactionId) {
		return false;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#tryResume(com.gemstone.gemfire.cache.TransactionId, long, java.util.concurrent.TimeUnit)
	 */
	@Override
	public boolean tryResume(TransactionId transactionId, long time, TimeUnit unit) {
		return false;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#exists(com.gemstone.gemfire.cache.TransactionId)
	 */
	@Override
	public boolean exists(TransactionId transactionId) {
		return false;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#exists()
	 */
	@Override
	public boolean exists() {
		return false;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#getTransactionId()
	 */
	@Override
	public TransactionId getTransactionId() {
		return null;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#getListener()
	 */
	@Override
	@Deprecated
	public TransactionListener getListener() {
		return null;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#getListeners()
	 */
	@Override
	public TransactionListener[] getListeners() {
		return listeners.toArray(new TransactionListener[listeners.size()]);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#setListener(com.gemstone.gemfire.cache.TransactionListener)
	 */
	@Override
	@Deprecated
	public TransactionListener setListener(TransactionListener newListener) {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#addListener(com.gemstone.gemfire.cache.TransactionListener)
	 */
	@Override
	public void addListener(TransactionListener aListener) {
		this.listeners.add(aListener);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#removeListener(com.gemstone.gemfire.cache.TransactionListener)
	 */
	@Override
	public void removeListener(TransactionListener aListener) {
		this.listeners.remove(aListener);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#initListeners(com.gemstone.gemfire.cache.TransactionListener[])
	 */
	@Override
	public void initListeners(TransactionListener[] newListeners) {
		this.listeners = Arrays.asList(newListeners);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#setWriter(com.gemstone.gemfire.cache.TransactionWriter)
	 */
	@Override
	public void setWriter(TransactionWriter writer) {
		this.writer = writer;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.CacheTransactionManager#getWriter()
	 */
	@Override
	public TransactionWriter getWriter() {
		return this.writer;
	}

	@Override
	public void setDistributed(final boolean b) {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	@Override
	public boolean isDistributed() {
		return false;
	}

}
