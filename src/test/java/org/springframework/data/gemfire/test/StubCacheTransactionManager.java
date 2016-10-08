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

import org.apache.geode.cache.CacheTransactionManager;
import org.apache.geode.cache.CommitConflictException;
import org.apache.geode.cache.TransactionId;
import org.apache.geode.cache.TransactionListener;
import org.apache.geode.cache.TransactionWriter;


/**
 * @author David Turanski
 *
 */
public class StubCacheTransactionManager implements CacheTransactionManager {

	private List<TransactionListener> listeners = new ArrayList<TransactionListener>();

	private TransactionWriter writer;

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#begin()
	 */
	@Override
	public void begin() {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#commit()
	 */
	@Override
	public void commit() throws CommitConflictException {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#rollback()
	 */
	@Override
	public void rollback() {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#suspend()
	 */
	@Override
	public TransactionId suspend() {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#resume(org.apache.geode.cache.TransactionId)
	 */
	@Override
	public void resume(TransactionId transactionId) {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#isSuspended(org.apache.geode.cache.TransactionId)
	 */
	@Override
	public boolean isSuspended(TransactionId transactionId) {
		return false;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#tryResume(org.apache.geode.cache.TransactionId)
	 */
	@Override
	public boolean tryResume(TransactionId transactionId) {
		return false;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#tryResume(org.apache.geode.cache.TransactionId, long, java.util.concurrent.TimeUnit)
	 */
	@Override
	public boolean tryResume(TransactionId transactionId, long time, TimeUnit unit) {
		return false;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#exists(org.apache.geode.cache.TransactionId)
	 */
	@Override
	public boolean exists(TransactionId transactionId) {
		return false;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#exists()
	 */
	@Override
	public boolean exists() {
		return false;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#getTransactionId()
	 */
	@Override
	public TransactionId getTransactionId() {
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#getListener()
	 */
	@Override
	@Deprecated
	public TransactionListener getListener() {
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#getListeners()
	 */
	@Override
	public TransactionListener[] getListeners() {
		return listeners.toArray(new TransactionListener[listeners.size()]);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#setListener(org.apache.geode.cache.TransactionListener)
	 */
	@Override
	@Deprecated
	public TransactionListener setListener(TransactionListener newListener) {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#addListener(org.apache.geode.cache.TransactionListener)
	 */
	@Override
	public void addListener(TransactionListener aListener) {
		this.listeners.add(aListener);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#removeListener(org.apache.geode.cache.TransactionListener)
	 */
	@Override
	public void removeListener(TransactionListener aListener) {
		this.listeners.remove(aListener);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#initListeners(org.apache.geode.cache.TransactionListener[])
	 */
	@Override
	public void initListeners(TransactionListener[] newListeners) {
		this.listeners = Arrays.asList(newListeners);
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#setWriter(org.apache.geode.cache.TransactionWriter)
	 */
	@Override
	public void setWriter(TransactionWriter writer) {
		this.writer = writer;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.CacheTransactionManager#getWriter()
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
