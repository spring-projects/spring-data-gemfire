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
import java.util.Set;
import java.util.concurrent.Executor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.core.task.TaskExecutor;
import org.springframework.data.gemfire.GemfireQueryException;
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
import com.gemstone.gemfire.internal.concurrent.ConcurrentHashSet;

/**
 * Container providing asynchronous behaviour for GemFire continuous queries.
 * 
 * @author Costin Leau
 */
public class ContinuousQueryListenerContainer implements InitializingBean, DisposableBean, BeanNameAware, SmartLifecycle {

	private class EventDispatcherAdapter implements CqListener {
		private final ContinuousQueryListener delegate;

		EventDispatcherAdapter(ContinuousQueryListener delegate) {
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

	/** Logger available to subclasses */
	protected final Log logger = LogFactory.getLog(getClass());

	/**
	 * Default thread name prefix: "ContinousQueryListenerContainer-".
	 */
	public static final String DEFAULT_THREAD_NAME_PREFIX = ClassUtils.getShortName(ContinuousQueryListenerContainer.class)
			+ "-";

	private Executor subscriptionExecutor;
	private Executor taskExecutor;
	private String beanName;
	private ErrorHandler errorHandler;

	// whether the container is running (or not)
	private volatile boolean running = false;
	// whether the container has been initialized
	private volatile boolean initialized = false;
	private volatile boolean manageExecutor = false;

	private Set<ContinuousQueryDefinition> defs = new LinkedHashSet<ContinuousQueryDefinition>();
	private Set<CqQuery> queries = new ConcurrentHashSet<CqQuery>();

	private QueryService queryService;
	private String poolName;


	public void afterPropertiesSet() {
		if (taskExecutor == null) {
			manageExecutor = true;
			taskExecutor = createDefaultTaskExecutor();
		}

		if (subscriptionExecutor == null) {
			subscriptionExecutor = taskExecutor;
		}

		if (StringUtils.hasText(poolName)) {
			Pool pool = PoolManager.find(poolName);
			Assert.notNull(pool, "No pool named [" + poolName + "] found");
			queryService = pool.getQueryService();
		}

		initMapping(defs);
		initialized = true;

		start();
	}

	/**
	 * Creates a default TaskExecutor. Called if no explicit TaskExecutor has been specified.
	 * <p>The default implementation builds a {@link org.springframework.core.task.SimpleAsyncTaskExecutor}
	 * with the specified bean name (or the class name, if no bean name specified) as thread name prefix.
	 * @see org.springframework.core.task.SimpleAsyncTaskExecutor#SimpleAsyncTaskExecutor(String)
	 */
	protected TaskExecutor createDefaultTaskExecutor() {
		String threadNamePrefix = (beanName != null ? beanName + "-" : DEFAULT_THREAD_NAME_PREFIX);
		return new SimpleAsyncTaskExecutor(threadNamePrefix);
	}

	public void destroy() throws Exception {
		initialized = false;

		stop();
		closeQueries();

		if (manageExecutor) {
			if (taskExecutor instanceof DisposableBean) {
				((DisposableBean) taskExecutor).destroy();

				if (logger.isDebugEnabled()) {
					logger.debug("Stopped internally-managed task executor");
				}
			}
		}
	}

	public boolean isAutoStartup() {
		return true;
	}

	public void stop(Runnable callback) {
		stop();
		callback.run();
	}

	public int getPhase() {
		// start the latest
		return Integer.MAX_VALUE;
	}

	public boolean isRunning() {
		return running;
	}

	public void start() {
		if (!running) {
			running = true;

			doStart();

			if (logger.isDebugEnabled()) {
				logger.debug("Started ContinousQueryListenerContainer");
			}
		}
	}

	public void stop() {
		if (running) {
			running = false;
			doStop();
		}

		if (logger.isDebugEnabled()) {
			logger.debug("Stopped ContinousQueryListenerContainer");
		}
	}

	private void doStart() {
		for (CqQuery cq : queries) {
			executeQuery(cq);
		}
	}

	private void doStop() {
		for (CqQuery cq : queries) {
			try {
				cq.stop();
			} catch (RuntimeException ex) {
				logger.warn("Cannot stop query", ex);
			} catch (QueryException ex) {
				logger.warn("Cannot stop query", ex);
			}
		}
	}

	private void closeQueries() {
		for (CqQuery cq : queries) {
			try {
				if (!cq.isClosed()) {
					cq.close();
				}
			} catch (QueryException ex) {
				logger.warn("Cannot close query", ex);
			} catch (RuntimeException ex) {
				logger.warn("Cannot close query", ex);
			}
		}

		queries.clear();
	}

	/**
	 * Execute the specified listener.
	 * 
	 * @see #handleListenerException
	 */
	protected void executeListener(ContinuousQueryListener listener, CqEvent event) {
		try {
			listener.onEvent(event);
		} catch (Throwable ex) {
			handleListenerException(ex);
		}
	}

	/**
	 * Return whether this container is currently active,
	 * that is, whether it has been set up but not shut down yet.
	 */
	public final boolean isActive() {
		return initialized;
	}

	/**
	 * Handle the given exception that arose during listener execution.
	 * <p>The default implementation logs the exception at error level.
	 * This can be overridden in subclasses.
	 * @param ex the exception to handle
	 */
	protected void handleListenerException(Throwable ex) {
		if (isActive()) {
			// Regular case: failed while active.
			// Invoke ErrorHandler if available.
			invokeErrorHandler(ex);
		}
		else {
			// Rare case: listener thread failed after container shutdown.
			// Log at debug level, to avoid spamming the shutdown logger.
			logger.debug("Listener exception after container shutdown", ex);
		}
	}

	/**
	 * Invoke the registered ErrorHandler, if any. Log at error level otherwise.
	 * @param ex the uncaught error that arose during event processing.
	 * @see #setErrorHandler
	 */
	protected void invokeErrorHandler(Throwable ex) {
		if (this.errorHandler != null) {
			this.errorHandler.handleError(ex);
		}
		else if (logger.isWarnEnabled()) {
			logger.warn("Execution of JMS event listener failed, and no ErrorHandler has been set.", ex);
		}
	}

	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Sets the task executor used for running the event listeners when messages are received.
	 * If no task executor is set, an instance of {@link SimpleAsyncTaskExecutor} will be used by default.
	 * The task executor can be adjusted depending on the work done by the listeners and the number of 
	 * messages coming in.
	 * 
	 * @param taskExecutor The taskExecutor to set.
	 */
	public void setTaskExecutor(Executor taskExecutor) {
		this.taskExecutor = taskExecutor;
	}

	/**
	 * Set an ErrorHandler to be invoked in case of any uncaught exceptions thrown
	 * while processing a event. By default there will be <b>no</b> ErrorHandler
	 * so that error-level logging is the only result.
	 */
	public void setErrorHandler(ErrorHandler errorHandler) {
		this.errorHandler = errorHandler;
	}

	/**
	 * Set the underlying cache used for registering queries.
	 * 
	 * @param cache cache used for registering queries
	 */
	public void setCache(RegionService cache) {
		this.queryService = cache.getQueryService();
	}

	/**
	 * Set the query service to be used by this container.
	 * 
	 * @param service query service used by the container
	 */
	public void setQueryService(QueryService service) {
		this.queryService = service;
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
		defs.clear();
		defs.addAll(queries);
	}

	/**
	 * Adds a query definition to the (potentially running) container. If the container is running,
	 * the listener starts receiving (matching) messages as soon as possible.
	 * 
	 * @param cqQuery cqQuery definition
	 */
	public void addListener(ContinuousQueryDefinition cqQuery) {
		doAddListener(cqQuery);
	}

	private void initMapping(Set<ContinuousQueryDefinition> queryDefinitions) {
		// stop the listener if currently running
		if (isRunning()) {
			stop();
		}

		closeQueries();

		for (ContinuousQueryDefinition def : queryDefinitions) {
			addCQuery(def);
		}

		// resume activity
		if (initialized) {
			start();
		}
	}

	private void doAddListener(ContinuousQueryDefinition def) {
		CqQuery cq = addCQuery(def);

		if (isRunning()) {
			executeQuery(cq);
		}
	}

	private CqQuery addCQuery(ContinuousQueryDefinition def) {
		try {
			CqAttributesFactory caf = new CqAttributesFactory();
			caf.addCqListener(new EventDispatcherAdapter(def.getListener()));
			CqAttributes attr = caf.create();

			CqQuery cq = null;

			if (StringUtils.hasText(def.getName())) {
				cq = queryService.newCq(def.getName(), def.getQuery(), attr, def.isDurable());
			}
			else {
				cq = queryService.newCq(def.getQuery(), attr, def.isDurable());
			}

			queries.add(cq);
			return cq;
		} catch (RuntimeException ex) {
			throw new GemfireQueryException("Cannot create query ", ex);
		} catch (QueryException ex) {
			throw new GemfireQueryException("Cannot create query ", ex);
		}
	}

	private void executeQuery(CqQuery cq) {
		try {
			cq.execute();
		} catch (QueryException ex) {
			throw new GemfireQueryException("Cannot execute query", ex);
		} catch (RuntimeException ex) {
			throw new GemfireQueryException("Cannot execute query", ex);
		}
	}

	private void dispatchEvent(final ContinuousQueryListener listener, final CqEvent event) {
		taskExecutor.execute(new Runnable() {
			public void run() {
				executeListener(listener, event);
			}
		});
	}
}