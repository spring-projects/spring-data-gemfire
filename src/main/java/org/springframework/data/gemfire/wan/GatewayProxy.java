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

import java.util.List;

import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.GatewayEventListener;

/**
 * This class used to allow decoupling of 'gateway' parsing from 'gateway-hub'
 * parsing
 * 
 * @author David Turanski
 * 
 */
public class GatewayProxy {

	private List<GatewayEndpoint> endpoints;

	private Integer concurrencyLevel = Gateway.DEFAULT_CONCURRENCY_LEVEL;

	private String id;

	private List<GatewayEventListener> listeners;

	private String orderPolicy;

	private int socketBufferSize = Gateway.DEFAULT_SOCKET_BUFFER_SIZE;

	private GatewayQueue queue;

	public void setEndpoints(List<GatewayEndpoint> endpoints) {
		this.endpoints = endpoints;
	}

	public void setListeners(List<GatewayEventListener> listeners) {
		this.listeners = listeners;
	}

	public void setQueue(GatewayQueue queue) {
		this.queue = queue;
	}

	public GatewayQueue getQueue() {
		return this.queue;
	}

	public Integer getConcurrencyLevel() {
		return this.concurrencyLevel;
	}

	public List<GatewayEndpoint> getEndpoints() {
		return endpoints;
	}

	public String getId() {
		return this.id;
	}

	public List<GatewayEventListener> getListeners() {
		return this.listeners;
	}

	public String getOrderPolicy() {
		return this.orderPolicy;
	}

	public Integer getSocketBufferSize() {
		return this.socketBufferSize;
	}
	
	public void setId(String id) {
		this.id = id;
	}

	public void setOrderPolicy(String orderPolicy) {
		this.orderPolicy = orderPolicy;
	}

	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;

	}

	public static class GatewayEndpoint {
		private String host;

		private String id;

		private int port;

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
		private Integer alertThreshold;

		private Boolean enableBatchConflation;

		private Integer batchTimeInterval;

		private Integer batchSize;

		private Boolean persistent;

		private String diskStoreRef;

		private Integer maximumQueueMemory;

		public Integer getAlertThreshold() {
			return alertThreshold;
		}

		public void setAlertThreshold(Integer alertThreshold) {
			this.alertThreshold = alertThreshold;
		}

		public Boolean getEnableBatchConflation() {
			return enableBatchConflation;
		}

		public void setEnableBatchConflation(Boolean enableBatchConflation) {
			this.enableBatchConflation = enableBatchConflation;
		}

		public Integer getBatchTimeInterval() {
			return batchTimeInterval;
		}

		public void setBatchTimeInterval(Integer batchTimeInterval) {
			this.batchTimeInterval = batchTimeInterval;
		}

		public Integer getBatchSize() {
			return batchSize;
		}

		public void setBatchSize(Integer batchSize) {
			this.batchSize = batchSize;
		}

		public Boolean getPersistent() {
			return persistent;
		}

		public void setPersistent(Boolean persistent) {
			this.persistent = persistent;
		}

		public String getDiskStoreRef() {
			return diskStoreRef;
		}

		public void setDiskStoreRef(String diskStoreRef) {
			this.diskStoreRef = diskStoreRef;
		}

		public Integer getMaximumQueueMemory() {
			return maximumQueueMemory;
		}

		public void setMaximumQueueMemory(Integer maximumQueueMemory) {
			this.maximumQueueMemory = maximumQueueMemory;
		}

	}
}
