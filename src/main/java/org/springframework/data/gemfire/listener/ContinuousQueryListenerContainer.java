/*
 * Copyright 2011-2013 the original author or authors.
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
 */

package org.springframework.data.gemfire.listener;

import java.util.LinkedHashSet;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.core.task.TaskExecutor;
import org.springframework.data.gemfire.GemfireQueryException;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.client.support.DefaultableDelegatingPoolAdapter;
import org.springframework.data.gemfire.client.support.DelegatingPoolAdapter;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.ErrorHandler;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.cache.query.CqAttributes;
import com.gemstone.gemfire.cache.query.CqAttributesFactory;
import com.gemstone.gemfire.cache.query.CqEvent;
import com.gemstone.gemfire.cache.query.CqListener;
import com.gemstone.gemfire.cache.query.CqQuery;
import com.gemstone.gemfire.cache.query.QueryException;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * Container providing asynchronous behaviour for GemFire continuous queries.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.context.SmartLifecycle
 * @see org.springframework.core.task.SimpleAsyncTaskExecutor
 * @see org.springframework.core.task.TaskExecutor
 * @see com.gemstone.gemfire.cache.RegionService
 * @see com.gemstone.gemfire.cache.client.Pool
 * @see com.gemstone.gemfire.cache.client.PoolManager
 * @see com.gemstone.gemfire.cache.query.CqEvent
 * @see com.gemstone.gemfire.cache.query.CqListener
 * @see com.gemstone.gemfire.cache.query.CqQuery
 * @see com.gemstone.gemfire.cache.query.QueryService
 */
@SuppressWarnings("unused")
public class ContinuousQueryListenerContainer implements BeanFactoryAware, BeanNameAware,
		InitializingBean, DisposableBean, SmartLifecycle {

	// Default Thread name prefix is "ContinuousQueryListenerContainer-".
	public static final String DEFAULT_THREAD_NAME_PREFIX = String.format("%1$s-", ClassUtils.getShortName(
		ContinuousQueryListenerContainer.class));

	private boolean autoStartup = true;

	private volatile boolean initialized = false;
	private volatile boolean manageExecutor = false;
	private volatile boolean running = false;

	private int phase = Integer.MAX_VALUE;

	private BeanFactory beanFactory;

	private ErrorHandler errorHandler;

	private Executor taskExecutor;

	protected final Log logger = LogFactory.getLog(getClass());

	private Queue<CqQuery> continuousQueries = new ConcurrentLinkedQueue<CqQuery>();

	private QueryService queryService;

	private Set<ContinuousQueryDefinition> continuousQueryDefinitions = new LinkedHashSet<ContinuousQueryDefinition>();

	private String beanName;
	private String poolName;

	public void afterPropertiesSet() {
		initQueryService(eagerlyInitializePool(resolvePoolName()));
		initExecutor();
		initContinuousQueries(continuousQueryDefinitions);

		Assert.state(queryService != null, "QueryService was not properly initialized");

		initialized = true;

		if (isAutoStartup()) {
			start();
		}
	}

	/* (non-Javadoc) */
	String resolvePoolName() {
		String poolName = this.poolName;

		if (!StringUtils.hasText(poolName)) {
			String defaultPoolName = GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME;
			poolName = (beanFactory != null && beanFactory.containsBean(defaultPoolName) ? defaultPoolName
				: GemfireUtils.DEFAULT_POOL_NAME);
		}

		return poolName;
	}

	/* (non-Javadoc) */
	String eagerlyInitializePool(String poolName) {
		try {
			if (beanFactory != null && beanFactory.isTypeMatch(poolName, Pool.class)) {
				beanFactory.getBean(poolName, Pool.class);
			}
		}
		catch (BeansException ignore) {
			Assert.notNull(PoolManager.find(poolName), String.format("No GemFire Pool with name [%1$s] was found",
				poolName));
		}

		return poolName;
	}

	/* (non-Javadoc) */
	QueryService initQueryService(String poolName) {
		if (queryService == null || StringUtils.hasText(poolName)) {
			queryService = DefaultableDelegatingPoolAdapter.from(DelegatingPoolAdapter.from(
				PoolManager.find(poolName))).preferPool().getQueryService(queryService);
		}

		return queryService;
	}

	/* (non-Javadoc) */
	Executor initExecutor() {
		if (taskExecutor == null) {
			taskExecutor = createDefaultTaskExecutor();
			manageExecutor = true;
		}

		return taskExecutor;
	}

	/**
	 * Creates a default TaskExecutor. Called if no explicit TaskExecutor has been configured.
	 * <p>The default implementation builds a {@link org.springframework.core.task.SimpleAsyncTaskExecutor}
	 * with the specified bean name (or the class name, if no bean name is specified) as thread name prefix.</p>
	 *
	 * @return an instance of the TaskExecutor used to process CQ events asynchronously.
	 * @see org.springframework.core.task.SimpleAsyncTaskExecutor#SimpleAsyncTaskExecutor(String)
	 */
	protected TaskExecutor createDefaultTaskExecutor() {
		return new SimpleAsyncTaskExecutor(beanName != null ? String.format("%1$s-", beanName)
			: DEFAULT_THREAD_NAME_PREFIX);
	}

	private void initContinuousQueries(Set<ContinuousQueryDefinition> continuousQueryDefinitions) {
		// stop the continuous query listener container if currently running...
		if (isRunning()) {
			stop();
		}

		// close any existing continuous queries...
		closeQueries();

		// add current continuous queries based on the definitions from the configuration...
		for (ContinuousQueryDefinition definition : continuousQueryDefinitions) {
			addContinuousQuery(definition);
		}
	}

	public synchronized void start() {
		if (!isRunning()) {
			doStart();
			running = true;

			if (logger.isDebugEnabled()) {
				logger.debug("Started ContinuousQueryListenerContainer");
			}
		}
	}

	private void doStart() {
		for (CqQuery cq : continuousQueries) {
			executeQuery(cq);
		}
	}

	private void executeQuery(CqQuery cq) {
		try {
			cq.execute();
		}
		catch (QueryException ex) {
			throw new GemfireQueryException(String.format("Could not execute query [%1$s]; state is [%2$s].",
				cq.getName(), cq.getState()), ex);
		}
		catch (RuntimeException ex) {
			throw new GemfireQueryException(String.format("Could not execute query [%1$s]; state is [%2$s].",
				cq.getName(), cq.getState()), ex);
		}
	}

	public synchronized void stop() {
		if (isRunning()) {
			doStop();
			running = false;
		}

		if (logger.isDebugEnabled()) {
			logger.debug("Stopped ContinuousQueryListenerContainer");
		}
	}

	public void stop(final Runnable callback) {
		stop();
		callback.run();
	}

	private void doStop() {
		for (CqQuery cq : continuousQueries) {
			try {
				cq.stop();
			}
			catch (Exception e) {
				logger.warn(String.format("Cannot stop query '%1$s'; state is '%2$s.", cq.getName(), cq.getState()), e);
			}
		}
	}

	public void destroy() throws Exception {
		stop();
		closeQueries();
		destroyExecutor();
		initialized = false;
	}

	private void closeQueries() {
		for (CqQuery cq : continuousQueries) {
			try {
				if (!cq.isClosed()) {
					cq.close();
				}
			}
			catch (Exception e) {
				logger.warn(String.format("Cannot close query '%1$s'; state is '%2$s.",
					cq.getName(), cq.getState()), e);
			}
		}

		continuousQueries.clear();
	}

	private void destroyExecutor() throws Exception {
		if (manageExecutor) {
			if (taskExecutor instanceof DisposableBean) {
				((DisposableBean) taskExecutor).destroy();

				if (logger.isDebugEnabled()) {
					logger.debug("Stopped internally-managed Task Executor.");
				}
			}
		}
	}

	/**
	 * Determines whether this container is currently active, that is, whether it has been setup (initialized)
	 * but not shutdown yet.
	 *
	 * @return a boolean indicating whether the container is active.
	 */
	public final boolean isActive() {
		return initialized;
	}

	/**
	 * Sets whether the CQ listener container should automatically start on startup.
	 *
	 * @param autoStartup a boolean value indicating whether this CQ listener container should automatically start.
	 */
	public void setAutoStartup(final boolean autoStartup) {
		this.autoStartup = autoStartup;
	}

	/**
	 * Determines whether this CQ listener container will automatically start on startup.
	 *
	 * @return a boolean value indicating whether this CQ listener container automatically starts.
	 * @see org.springframework.context.SmartLifecycle#isAutoStartup()
	 */
	public boolean isAutoStartup() {
		return autoStartup;
	}

	/**
	 * Determines whether the container has be started and is currently running.
	 *
	 * @return a boolean value indicating whether the container has been started and is currently running.
	 */
	public synchronized boolean isRunning() {
		return running;
	}

	/**
	 * Sets the {@link BeanFactory} containing this bean.
	 *
	 * @param beanFactory the Spring {@link BeanFactory} containing this bean.
	 * @throws BeansException if an initialization error occurs.
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Set the name of the bean in the bean factory that created this bean.
	 * <p>Invoked after population of normal bean properties but before an
	 * init callback such as {@link InitializingBean#afterPropertiesSet()}
	 * or a custom init-method.</p>
	 *
	 * @param name the name of the bean in the factory.
	 */
	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Set the underlying RegionService (GemFire Cache) used for registering Queries.
	 *
	 * @param cache the RegionService (GemFire Cache) used for registering Queries.
	 * @see com.gemstone.gemfire.cache.RegionService
	 */
	public void setCache(RegionService cache) {
		setQueryService(cache.getQueryService());
	}

	/**
	 * Set an ErrorHandler to be invoked in case of any uncaught exceptions thrown while processing a CQ event.
	 * By default there will be <b>no</b> ErrorHandler so that error-level logging is the only result.
	 *
	 * @param errorHandler the ErrorHandler invoked when uncaught exceptions are thrown while processing the CQ event.
	 * @see org.springframework.util.ErrorHandler
	 */
	public void setErrorHandler(ErrorHandler errorHandler) {
		this.errorHandler = errorHandler;
	}

	/**
	 * Sets the phase in which this CQ listener container will start in the Spring container.
	 *
	 * @param phase the phase value of this CQ listener container.
	 */
	public void setPhase(final int phase) {
		this.phase = phase;
	}

	/**
	 * Gets the phase in which this CQ listener container will start in the Spring container.
	 *
	 * @return the phase value of this CQ listener container.
	 * @see org.springframework.context.Phased#getPhase()
	 */
	public int getPhase() {
		return phase;
	}

	/**
	 * Set the name of the {@link Pool} used for performing the queries by this container.
	 *
	 * @param poolName the name of the pool to be used by the container
	 */
	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	/**
	 * Attaches the given query definitions.
	 *
	 * @param queries set of queries
	 */
	public void setQueryListeners(Set<ContinuousQueryDefinition> queries) {
		continuousQueryDefinitions.clear();
		continuousQueryDefinitions.addAll(queries);
	}

	/**
	 * Set the GemFire QueryService used by this container to create ContinuousQueries (CQ).
	 *
	 * @param service the GemFire QueryService object used by the container to create ContinuousQueries (CQ).
	 * @see com.gemstone.gemfire.cache.query.QueryService
	 */
	public void setQueryService(QueryService service) {
		this.queryService = service;
	}

	/**
	 * Sets the Task Executor used for running the event listeners when messages are received.
	 * If no task executor is set, an instance of {@link SimpleAsyncTaskExecutor} will be used by default.
	 * The task executor can be adjusted depending on the work done by the listeners and the number of
	 * messages coming in.
	 *
	 * @param taskExecutor The Task Executor used to run event listeners when query results messages are received.
	 * @see java.util.concurrent.Executor
	 */
	public void setTaskExecutor(Executor taskExecutor) {
		this.taskExecutor = taskExecutor;
	}

	/**
	 * Adds a Continuous Query (CQ) definition to the (potentially running) container. If the container is running,
	 * the listener starts receiving (matching) messages as soon as possible.
	 *
	 * @param definition Continuous Query (CQ) definition
	 * @see org.springframework.data.gemfire.listener.ContinuousQueryDefinition
	 * @see #doAddListener(ContinuousQueryDefinition)
	 */
	public void addListener(ContinuousQueryDefinition definition) {
		doAddListener(definition);
	}

	private void doAddListener(ContinuousQueryDefinition definition) {
		CqQuery cq = addContinuousQuery(definition);

		if (isRunning()) {
			executeQuery(cq);
		}
	}

	private CqQuery addContinuousQuery(ContinuousQueryDefinition definition) {
		try {
			CqAttributesFactory continuousQueryAttributesFactory = new CqAttributesFactory();

			continuousQueryAttributesFactory.addCqListener(new EventDispatcherAdapter(definition.getListener()));

			CqAttributes continuousQueryAttributes = continuousQueryAttributesFactory.create();

			CqQuery cq =  (StringUtils.hasText(definition.getName())
				? queryService.newCq(definition.getName(), definition.getQuery(), continuousQueryAttributes, definition.isDurable())
				: queryService.newCq(definition.getQuery(), continuousQueryAttributes, definition.isDurable()));

			continuousQueries.add(cq);

			return cq;
		}
		catch (RuntimeException ex) {
			throw new GemfireQueryException("Cannot create query ", ex);
		}
		catch (QueryException ex) {
			throw new GemfireQueryException("Cannot create query ", ex);
		}
	}

	private void dispatchEvent(final ContinuousQueryListener listener, final CqEvent event) {
		taskExecutor.execute(new Runnable() {
			public void run() {
				executeListener(listener, event);
			}
		});
	}

	/**
	 * Execute the specified listener.
	 *
	 * @param listener the ContinuousQueryListener to notify of the CQ event.
	 * @param event the CQ event.
	 * @see #handleListenerException(Throwable)
	 */
	protected void executeListener(ContinuousQueryListener listener, CqEvent event) {
		try {
			listener.onEvent(event);
		}
		catch (Throwable ex) {
			handleListenerException(ex);
		}
	}

	/**
	 * Handle the given exception that arose during listener execution.
	 * <p>The default implementation logs the exception at error level.
	 * This can be overridden in subclasses.
	 *
	 * @param e the exception to handle
	 */
	protected void handleListenerException(Throwable e) {
		if (isActive()) {
			// Regular case: failed while active.
			// Invoke ErrorHandler if available.
			invokeErrorHandler(e);
		}
		else {
			// Rare case: listener thread failed after container shutdown.
			// Log at debug level, to avoid spamming the shutdown logger.
			logger.debug("Listener exception after container shutdown", e);
		}
	}

	/**
	 * Invoke the registered ErrorHandler, if any. Log at error level otherwise.
	 *
	 * @param e the uncaught error that arose during event processing.
	 * @see #setErrorHandler
	 */
	protected void invokeErrorHandler(Throwable e) {
		if (this.errorHandler != null) {
			this.errorHandler.handleError(e);
		}
		else if (logger.isWarnEnabled()) {
			logger.warn("Execution of the CQ event listener failed, and no ErrorHandler has been set.", e);
		}
	}

	private class EventDispatcherAdapter implements CqListener {

		private final ContinuousQueryListener delegate;

		private EventDispatcherAdapter(final ContinuousQueryListener delegate) {
			this.delegate = delegate;
		}

		public void onError(CqEvent event) {
			dispatchEvent(delegate, event);
		}

		public void onEvent(CqEvent event) {
			dispatchEvent(delegate, event);
		}

		public void close() {
		}
	}

}
