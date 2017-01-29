/*
 * Copyright 2016-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.transaction;

import static org.springframework.data.gemfire.transaction.GemfireTransactionManager.CacheHolder.newCacheHolder;
import static org.springframework.data.gemfire.transaction.GemfireTransactionManager.CacheTransactionObject.newCacheTransactionObject;

import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.apache.geode.cache.CacheTransactionManager;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.TransactionId;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.transaction.CannotCreateTransactionException;
import org.springframework.transaction.NoTransactionException;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionException;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;
import org.springframework.transaction.support.DefaultTransactionStatus;
import org.springframework.transaction.support.ResourceTransactionManager;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.util.Assert;

/**
 * Local Transaction Management for Pivotal GemFire. Provides a Spring {@link PlatformTransactionManager} implementation
 * for the Pivotal GemFire {@link CacheTransactionManager}.
 *
 * Binds one or multiple GemFire {@link Region Regions} for the specified {@link GemFireCache} to the thread,
 * potentially allowing for one {@link Region} per {@link GemFireCache} model.
 *
 * <p>
 * This local strategy is an alternative to executing cache operations within JTA transactions.
 * Its advantage is that is able to work in any environment, for example a stand-alone application
 * or a test suite. It is <i>not</i> able to provide XA transactions, for example to share transactions
 * with data access.
 *
 * <p>
 * By default, to prevent dirty reads, the {@link GemFireCache} is configured to return copies rather then direct references
 * for <code>get</code> data access operations. As a workaround, one could use explicitly deep copy objects before
 * making changes to them to avoid unnecessary copying on every fetch.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.apache.geode.CopyHelper#copy(Object)
 * @see org.apache.geode.cache.GemFireCache#setCopyOnRead(boolean)
 * @see org.apache.geode.cache.CacheTransactionManager
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.TransactionId
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.transaction.PlatformTransactionManager
 * @see org.springframework.transaction.TransactionDefinition
 * @see org.springframework.transaction.support.AbstractPlatformTransactionManager
 * @see org.springframework.transaction.support.ResourceTransactionManager
 * @see org.springframework.transaction.support.TransactionSynchronizationManager
 * @see #setCopyOnRead(boolean)
 */
@SuppressWarnings("unused")
public class GemfireTransactionManager extends AbstractPlatformTransactionManager
		implements InitializingBean, ResourceTransactionManager {

	protected static final TimeUnit DEFAULT_RESUME_WAIT_TIME_UNIT = TimeUnit.SECONDS;

	private GemFireCache cache;

	private boolean copyOnRead = true;

	private Long resumeWaitTime;

	private TimeUnit resumeWaitTimeUnit = DEFAULT_RESUME_WAIT_TIME_UNIT;

	/**
	 * Constructs an instance of the {@link GemfireTransactionManager}.
	 */
	public GemfireTransactionManager() {
	}

	/**
	 * Constructs an instance of the {@link GemfireTransactionManager} initialized with
	 * the given {@link GemFireCache} reference.
	 *
	 * @param cache reference to the {@link GemFireCache} associated with cache transactions.
	 * @see org.apache.geode.cache.GemFireCache
	 * @see #afterPropertiesSet()
	 */
	public GemfireTransactionManager(GemFireCache cache) {
		this.cache = cache;
		afterPropertiesSet();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterPropertiesSet() {
		Assert.notNull(this.cache, "Cache is required");
		this.cache.setCopyOnRead(isCopyOnRead());
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected Object doGetTransaction() throws TransactionException {
		return newCacheTransactionObject((CacheHolder) TransactionSynchronizationManager.getResource(getCache()));
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected boolean isExistingTransaction(Object transaction) throws TransactionException {
		// consider a pre-bound cache as a transaction
		return ((CacheTransactionObject) transaction).isHolding();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doBegin(Object transaction, TransactionDefinition definition) throws TransactionException {
		try {
			CacheTransactionObject cacheTransaction = (CacheTransactionObject) transaction;
			GemFireCache cache = getCache();

			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Acquired GemFire Cache [%s] for local cache transaction", cache));
			}

			CacheTransactionManager cacheTransactionManager = getCacheTransactionManager();

			// begin GemFire local cache transaction
			cacheTransactionManager.begin();

			TransactionId transactionId = cacheTransactionManager.getTransactionId();

			if (transactionId != null) {
				TransactionSynchronizationManager.bindResource(cache,
					cacheTransaction.setAndGetHolder(newCacheHolder(transactionId)));
			}
		}
		catch (IllegalStateException e) {
			throw new CannotCreateTransactionException(String.format("%1$s; %2$s",
				"An existing, ongoing transaction is already associated with the current thread",
				"are multiple transaction managers present?"), e);
		}
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doCommit(DefaultTransactionStatus status) throws TransactionException {
		try {
			if (status.isDebug()) {
				logger.debug("Committing local cache transaction");
			}

			getCacheTransactionManager().commit();
		}
		catch (IllegalStateException e) {
			throw new NoTransactionException(
				"No transaction is associated with the current thread; are multiple transaction managers present?", e);
		}
		catch (org.apache.geode.cache.TransactionException e) {
			throw new GemfireTransactionCommitException(
				"Unexpected failure occurred on commit of local cache transaction", e);
		}
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected Object doSuspend(Object transaction) throws TransactionException {
		if (getCacheTransactionManager().suspend() != null) {
			TransactionSynchronizationManager.unbindResource(getCache());
			return ((CacheTransactionObject) transaction).setAndGetExistingHolder(null);
		}

		return null;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doResume(Object transaction, Object suspendedResources) throws TransactionException {
		if (suspendedResources instanceof CacheHolder) {
			CacheHolder holder = (CacheHolder) suspendedResources;

			boolean resumeSuccessful = (isResumeWaitTimeSet()
				? getCacheTransactionManager().tryResume(holder.getTransactionId(),
				getResumeWaitTime(), getResumeWaitTimeUnit())
				: getCacheTransactionManager().tryResume(holder.getTransactionId()));

			if (resumeSuccessful) {
				TransactionSynchronizationManager.bindResource(getCache(),
					((CacheTransactionObject) transaction).setAndGetHolder(holder));
			}
		}
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doRollback(DefaultTransactionStatus status) throws TransactionException {
		try {
			if (status.isDebug()) {
				logger.debug("Rolling back local cache transaction");
			}

			getCacheTransactionManager().rollback();
		}
		catch (IllegalStateException e) {
			throw new NoTransactionException(
				"No transaction is associated with the current thread; are multiple transaction managers present?", e);
		}
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doCleanupAfterCompletion(Object transaction) {
		TransactionSynchronizationManager.unbindResource(getCache());
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doSetRollbackOnly(DefaultTransactionStatus status) {
		((CacheTransactionObject) status.getTransaction()).getHolder().setRollbackOnly();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected final boolean useSavepointForNestedTransaction() {
		return false;
	}

	/**
	 * Sets a reference to the {@link GemFireCache} for which this transaction manager
	 * will manage local cache transactions.
	 * @param cache reference to the {@link GemFireCache}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	public void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	/**
	 * Returns a reference to the {@link GemFireCache} for which this transaction manager
	 * will manage local cache transactions.
	 *
	 * @return a reference to the {@link GemFireCache}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	public GemFireCache getCache() {
		return this.cache;
	}

	protected CacheTransactionManager getCacheTransactionManager() {
		return getCache().getCacheTransactionManager();
	}

	/**
	 * Set whether the cache returns direct object references or copies of the objects it manages.
	 * While copies imply additional work for every fetch operation, direct object references can
	 * cause dirty reads across concurrent threads in the same VM, whether or not transactions are used.
	 *
	 * One could explicitly deep copy objects before making changes (for example by using
	 * {@link org.apache.geode.CopyHelper#copy(Object)} in which case this setting
	 * can be set to <code>false</code>
	 *
	 * However, unless there is a measurable performance penalty, the recommendation is
	 * to keep this setting to <code>true</code>.
	 *
	 * @param copyOnRead boolean value indicating whether copies (default) rather then direct object references
	 * will be returned on fetch operations.
	 */
	public void setCopyOnRead(boolean copyOnRead) {
		this.copyOnRead = copyOnRead;
	}

	/**
	 * Indicates whether copy on read is set and used for fetch data access operations.
	 *
	 * @return the setting for copy-on-read.
	 * @see #setCopyOnRead(boolean)
	 */
	public boolean isCopyOnRead() {
		return copyOnRead;
	}

	/**
	 * Sets the GemFire cache {@link Region} as an alternative in setting in the {@link GemFireCache} directly.
	 *
	 * @param <K> {@link Class} type of the {@link Region} key.
	 * @param <V> {@link Class} type of the {@link Region} value.
	 * @param region GemFire cache {@link Region} directly involved in the local cache transaction.
	 * @throws IllegalArgumentException if {@link Region} is {@literal null}.
	 * @see org.apache.geode.cache.Region
	 */
	public <K, V> void setRegion(Region<K, V> region) {
		Assert.notNull(region, "Region must not be null");
		this.cache = (GemFireCache) region.getRegionService();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Object getResourceFactory() {
		return getCache();
	}

	/***
	 * Sets the timeout used to wait for the GemFire cache transaction to resume.
	 *
	 * @param resumeWaitTime long value with the timeout used to wait for the GemFire cache transaction to resume.
	 * @see org.apache.geode.cache.CacheTransactionManager#tryResume(TransactionId, long, TimeUnit)
	 */
	public void setResumeWaitTime(Long resumeWaitTime) {
		this.resumeWaitTime = resumeWaitTime;
	}

	/***
	 * Returns the timeout used to wait for the GemFire cache transaction to resume.
	 *
	 * @return the long value with the timeout used to wait for the GemFire cache transaction to resume.
	 * @see org.apache.geode.cache.CacheTransactionManager#tryResume(TransactionId, long, TimeUnit)
	 */
	protected Long getResumeWaitTime() {
		return this.resumeWaitTime;
	}

	/**
	 * Determines whether the user specified a wait time for resuming a GemFire cache transaction.
	 *
	 * @return a boolean value to indicate whether the user specified a wait time
	 * for resuming a GemFire cache transaction.
	 * @see org.apache.geode.cache.CacheTransactionManager#tryResume(TransactionId, long, TimeUnit)
	 * @see #getResumeWaitTime()
	 */
	protected boolean isResumeWaitTimeSet() {
		Long resumeWaitTime = getResumeWaitTime();
		return (resumeWaitTime != null && resumeWaitTime > 0);
	}

	/**
	 * Sets the {@link TimeUnit} used in the wait timeout when resuming a GemFire cache transaction.
	 *
	 * @param resumeWaitTimeUnit {@link TimeUnit} used in the wait timeout when resuming a GemFire cache transaction.
	 * @see org.apache.geode.cache.CacheTransactionManager#tryResume(TransactionId, long, TimeUnit)
	 */
	public void setResumeWaitTimeUnit(TimeUnit resumeWaitTimeUnit) {
		this.resumeWaitTimeUnit = resumeWaitTimeUnit;
	}

	/**
	 * Returns the {@link TimeUnit} used in the wait timeout when resuming a GemFire cache transaction.
	 *
	 * Defaults to {@link TimeUnit#SECONDS}.
	 *
	 * @return the {@link TimeUnit} used in the wait timeout when resuming a GemFire cache transaction.
	 * @see org.apache.geode.cache.CacheTransactionManager#tryResume(TransactionId, long, TimeUnit)
	 */
	protected TimeUnit getResumeWaitTimeUnit() {
		return Optional.ofNullable(this.resumeWaitTimeUnit).orElse(DEFAULT_RESUME_WAIT_TIME_UNIT);
	}

	/**
	 * GemFire local transaction object.
	 *
	 * @author Costin Leau
	 * @author John Blum
	 */
	protected static class CacheTransactionObject {

		private CacheHolder cacheHolder;

		/* (non-Javadoc) */
		static CacheTransactionObject newCacheTransactionObject(CacheHolder cacheHolder) {
			CacheTransactionObject transactionObject = new CacheTransactionObject();
			transactionObject.setHolder(cacheHolder);
			return transactionObject;
		}

		/* (non-Javadoc) */
		boolean isHolding() {
			return (getHolder() != null);
		}

		/* (non-Javadoc) */
		CacheHolder getHolder() {
			return this.cacheHolder;
		}

		/* (non-Javadoc) */
		void setHolder(CacheHolder holder) {
			this.cacheHolder = holder;
		}

		/* (non-Javadoc) */
		CacheHolder setAndGetExistingHolder(CacheHolder cacheHolder) {
			CacheHolder existingHolder = getHolder();
			setHolder(cacheHolder);
			return existingHolder;
		}

		/* (non-Javadoc) */
		CacheHolder setAndGetHolder(CacheHolder holder) {
			setHolder(holder);
			return getHolder();
		}
	}

	/**
	 * Holder of GemFire cache transaction state.
	 */
	protected static class CacheHolder {

		private boolean rollbackOnly = false;

		private TransactionId transactionId;

		/* (non-Javadoc) */
		static CacheHolder newCacheHolder(TransactionId transactionId) {
			CacheHolder cacheHolder = new CacheHolder();
			cacheHolder.transactionId = transactionId;
			return cacheHolder;
		}

		/* (non-Javadoc) */
		boolean isRollbackOnly() {
			return this.rollbackOnly;
		}

		/* (non-Javadoc) */
		void setRollbackOnly() {
			this.rollbackOnly = true;
		}

		/* (non-Javadoc) */
		TransactionId getTransactionId() {
			return this.transactionId;
		}
	}
}
