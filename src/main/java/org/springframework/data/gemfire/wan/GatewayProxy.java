/*
 * Copyright 2010-2013 the original author or authors.
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
package org.springframework.data.gemfire.wan;

import java.util.Collections;
import java.util.List;

import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.GatewayEventListener;
import com.gemstone.gemfire.cache.util.GatewayQueueAttributes;

/**
 * The GatewayProxy class used to allow decoupling of 'gateway' parsing from 'gateway-hub' parsing.
 * 
 * @author David Turanski
 * @author John Blum
 * @since com.gemstone.gemfire.cache.util.Gateway
 */
@SuppressWarnings({ "deprecation", "unused" })
public class GatewayProxy {

	private Gateway.OrderPolicy orderPolicy;

	private GatewayQueue queue;

	private Integer concurrencyLevel = Gateway.DEFAULT_CONCURRENCY_LEVEL;
	private Integer socketBufferSize = Gateway.DEFAULT_SOCKET_BUFFER_SIZE;
	private Integer socketReadTimeout = Gateway.DEFAULT_SOCKET_READ_TIMEOUT;

	private List<GatewayEndpoint> endpoints;
	private List<GatewayEventListener> listeners;

	private String id;

	public void setConcurrencyLevel(Integer concurrencyLevel) {
		this.concurrencyLevel = concurrencyLevel;
	}

	public Integer getConcurrencyLevel() {
		return (concurrencyLevel != null ? concurrencyLevel : Gateway.DEFAULT_CONCURRENCY_LEVEL);
	}

	public void setEndpoints(List<GatewayEndpoint> endpoints) {
		this.endpoints = endpoints;
	}

	public List<GatewayEndpoint> getEndpoints() {
		return (endpoints != null ? endpoints : Collections.<GatewayEndpoint>emptyList());
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getId() {
		return this.id;
	}

	public void setListeners(List<GatewayEventListener> listeners) {
		this.listeners = listeners;
	}

	public List<GatewayEventListener> getListeners() {
		return (listeners != null ? listeners : Collections.<GatewayEventListener>emptyList());
	}

	public void setOrderPolicy(Gateway.OrderPolicy orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	public Gateway.OrderPolicy getOrderPolicy() {
		return this.orderPolicy;
	}

	public void setQueue(GatewayQueue queue) {
		this.queue = queue;
	}

	public GatewayQueue getQueue() {
		return this.queue;
	}

	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public Integer getSocketBufferSize() {
		return (socketBufferSize != null ? socketBufferSize : Gateway.DEFAULT_SOCKET_BUFFER_SIZE);
	}

	public void setSocketReadTimeout(final Integer socketReadTimeout) {
		this.socketReadTimeout = socketReadTimeout;
	}

	public Integer getSocketReadTimeout() {
		return (socketReadTimeout != null ? socketReadTimeout : Gateway.DEFAULT_SOCKET_READ_TIMEOUT);
	}

	public static class GatewayEndpoint {

		private int port;

		private String host;
		private String id;

		public String getHost() {
			return host;
		}

		public void setHost(String host) {
			this.host = host;
		}

		public String getId() {
			return id;
		}

		public void setId(String id) {
			this.id = id;
		}

		public int getPort() {
			return port;
		}

		public void setPort(int port) {
			this.port = port;
		}
	}

	public static class GatewayQueue {

		private Boolean enableBatchConflation;
		private Boolean persistent;

		private Integer alertThreshold;
		private Integer batchSize;
		private Integer batchTimeInterval;
		private Integer maximumQueueMemory;

		private String diskStoreRef;

		public void setAlertThreshold(Integer alertThreshold) {
			this.alertThreshold = alertThreshold;
		}

		public Integer getAlertThreshold() {
			return (alertThreshold != null ? alertThreshold : GatewayQueueAttributes.DEFAULT_ALERT_THRESHOLD);
		}

		public void setBatchSize(Integer batchSize) {
			this.batchSize = batchSize;
		}

		public Integer getBatchSize() {
			return (batchSize != null ? batchSize : GatewayQueueAttributes.DEFAULT_BATCH_SIZE);
		}

		public void setBatchTimeInterval(Integer batchTimeInterval) {
			this.batchTimeInterval = batchTimeInterval;
		}

		public Integer getBatchTimeInterval() {
			return (batchTimeInterval != null ? batchTimeInterval : GatewayQueueAttributes.DEFAULT_BATCH_TIME_INTERVAL);
		}

		public void setDiskStoreRef(String diskStoreRef) {
			this.diskStoreRef = diskStoreRef;
		}

		public String getDiskStoreRef() {
			return diskStoreRef;
		}

		public void setEnableBatchConflation(Boolean enableBatchConflation) {
			this.enableBatchConflation = enableBatchConflation;
		}

		public Boolean getEnableBatchConflation() {
			return (enableBatchConflation != null ? enableBatchConflation
				: GatewayQueueAttributes.DEFAULT_BATCH_CONFLATION);
		}

		public void setMaximumQueueMemory(Integer maximumQueueMemory) {
			this.maximumQueueMemory = maximumQueueMemory;
		}

		public Integer getMaximumQueueMemory() {
			return (maximumQueueMemory != null ? maximumQueueMemory
				: GatewayQueueAttributes.DEFAULT_MAXIMUM_QUEUE_MEMORY);
		}

		public void setPersistent(Boolean persistent) {
			this.persistent = persistent;
		}

		public Boolean getPersistent() {
			return (persistent != null ? persistent : GatewayQueueAttributes.DEFAULT_ENABLE_PERSISTENCE);
		}
	}

}
