/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
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

public class StubCacheTransactionManager implements CacheTransactionManager {

	private boolean distributed = false;

	private List<TransactionListener> listeners = new ArrayList<>();

	private TransactionWriter writer;

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isSuspended(TransactionId transactionId) {
		return false;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void begin() {
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void commit() throws CommitConflictException {

	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean exists() {
		return false;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean exists(TransactionId transactionId) {
		return false;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void resume(TransactionId transactionId) {
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void rollback() {
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public TransactionId suspend() {
		return null;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean tryResume(TransactionId transactionId) {
		return false;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean tryResume(TransactionId transactionId, long time, TimeUnit unit) {
		return false;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public TransactionId getTransactionId() {
		return null;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	@Deprecated
	public TransactionListener getListener() {
		return null;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public TransactionListener[] getListeners() {
		return listeners.toArray(new TransactionListener[listeners.size()]);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	@Deprecated
	public TransactionListener setListener(TransactionListener newListener) {
		return null;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void addListener(TransactionListener aListener) {
		this.listeners.add(aListener);

	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void removeListener(TransactionListener aListener) {
		this.listeners.remove(aListener);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void initListeners(TransactionListener[] newListeners) {
		this.listeners = Arrays.asList(newListeners);

	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setWriter(TransactionWriter writer) {
		this.writer = writer;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public TransactionWriter getWriter() {
		return this.writer;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setDistributed(boolean distributed) {
		this.distributed = distributed;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isDistributed() {
		return this.distributed;
	}
}
