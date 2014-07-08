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

import java.io.IOException;
import java.util.Set;

import com.gemstone.gemfire.cache.ClientSession;
import com.gemstone.gemfire.cache.InterestRegistrationListener;
import com.gemstone.gemfire.cache.server.CacheServer;
import com.gemstone.gemfire.cache.server.ClientSubscriptionConfig;
import com.gemstone.gemfire.cache.server.ServerLoadProbe;
import com.gemstone.gemfire.distributed.DistributedMember;

/**
 * @author David Turanski
 * @author John Blum
 */
@SuppressWarnings("deprecation")
public class StubCacheServer implements CacheServer {

	private boolean isRunning;
	private boolean notifyBySubscription;
	private boolean tcpNoDelay;

	private int maxConnections;
	private int maximumMessageCount;
	private int maximumTimeBetweenPings;
	private int maxThreads;
	private int messageTimeToLive;
	private int port;
	private int socketBufferSize;

	private long loadPollInterval;

	private ClientSession clientSession;

	private ClientSubscriptionConfig clientSubscriptionConfig = mockClientSubscriptionConfig();

	private Set<ClientSession> clientSessions;

	private ServerLoadProbe serverLoadProbe;

	private String bindAddress;
	private String hostNameForClients;
	private String[] groups;

		/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getPort()
	 */
	@Override
	public int getPort() {
		return port;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setPort(int)
	 */
	@Override
	public void setPort(int port) {
		this.port = port;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getBindAddress()
	 */
	@Override
	public String getBindAddress() {
		return bindAddress;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setBindAddress(java.lang.String)
	 */
	@Override
	public void setBindAddress(String address) {
		this.bindAddress = address;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getHostnameForClients()
	 */
	@Override
	public String getHostnameForClients() {
		return this.hostNameForClients;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setHostnameForClients(java.lang.String)
	 */
	@Override
	public void setHostnameForClients(String name) {
		this.hostNameForClients = name;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setNotifyBySubscription(boolean)
	 */
	@Override
	@Deprecated
	public void setNotifyBySubscription(boolean b) {
		this.notifyBySubscription = b;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getNotifyBySubscription()
	 */
	@Override
	@Deprecated
	public boolean getNotifyBySubscription() {
		return this.notifyBySubscription;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setSocketBufferSize(int)
	 */
	@Override
	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getSocketBufferSize()
	 */
	@Override
	public int getSocketBufferSize() {
		return this.socketBufferSize;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setMaximumTimeBetweenPings(int)
	 */
	@Override
	public void setMaximumTimeBetweenPings(int maximumTimeBetweenPings) {
		this.maximumTimeBetweenPings = maximumTimeBetweenPings;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getMaximumTimeBetweenPings()
	 */
	@Override
	public int getMaximumTimeBetweenPings() {
		return this.maximumTimeBetweenPings;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#start()
	 */
	@Override
	public void start() throws IOException {
		isRunning = true;
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#isRunning()
	 */
	@Override
	public boolean isRunning() {
 		return isRunning;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#stop()
	 */
	@Override
	public void stop() {
		isRunning = false;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getMaxConnections()
	 */
	@Override
	public int getMaxConnections() {
		return this.maxConnections;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setMaxConnections(int)
	 */
	@Override
	public void setMaxConnections(int maxCons) {
		this.maxConnections = maxCons;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getMaxThreads()
	 */
	@Override
	public int getMaxThreads() {
		return this.maxThreads;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setMaxThreads(int)
	 */
	@Override
	public void setMaxThreads(int maxThreads) {
		this.maxThreads = maxThreads;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getMaximumMessageCount()
	 */
	@Override
	public int getMaximumMessageCount() {
		return this.maximumMessageCount;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setMaximumMessageCount(int)
	 */
	@Override
	public void setMaximumMessageCount(int maxMessageCount) {
		this.maximumMessageCount = maxMessageCount;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getMessageTimeToLive()
	 */
	@Override
	public int getMessageTimeToLive() {
		return this.messageTimeToLive;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setMessageTimeToLive(int)
	 */
	@Override
	public void setMessageTimeToLive(int messageTimeToLive) {
		this.messageTimeToLive = messageTimeToLive;
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setGroups(java.lang.String[])
	 */
	@Override
	public void setGroups(String[] groups) {
		this.groups = groups;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getGroups()
	 */
	@Override
	public String[] getGroups() {
		return this.groups;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getLoadProbe()
	 */
	@Override
	public ServerLoadProbe getLoadProbe() {
		return this.serverLoadProbe;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setLoadProbe(com.gemstone.gemfire.cache.server.ServerLoadProbe)
	 */
	@Override
	public void setLoadProbe(ServerLoadProbe loadProbe) {
		this.serverLoadProbe = loadProbe;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getLoadPollInterval()
	 */
	@Override
	public long getLoadPollInterval() {
		return this.loadPollInterval;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setLoadPollInterval(long)
	 */
	@Override
	public void setLoadPollInterval(long loadPollInterval) {
		this.loadPollInterval = loadPollInterval;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getClientSubscriptionConfig()
	 */
	@Override
	public ClientSubscriptionConfig getClientSubscriptionConfig() {
		return this.clientSubscriptionConfig;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getClientSession(com.gemstone.gemfire.distributed.DistributedMember)
	 */
	@Override
	public ClientSession getClientSession(DistributedMember member) {
		return this.clientSession;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getClientSession(java.lang.String)
	 */
	@Override
	public ClientSession getClientSession(String durableClientId) {
		return this.clientSession;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getAllClientSessions()
	 */
	@Override
	public Set<ClientSession> getAllClientSessions() {
		return this.clientSessions;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#registerInterestRegistrationListener(com.gemstone.gemfire.cache.InterestRegistrationListener)
	 */
	@Override
	public void registerInterestRegistrationListener(InterestRegistrationListener listener) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#unregisterInterestRegistrationListener(com.gemstone.gemfire.cache.InterestRegistrationListener)
	 */
	@Override
	public void unregisterInterestRegistrationListener(InterestRegistrationListener listener) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getInterestRegistrationListeners()
	 */
	@Override
	public Set<InterestRegistrationListener> getInterestRegistrationListeners() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#getTcpNoDelay()
	 */
	@Override
	public boolean getTcpNoDelay() {
		return tcpNoDelay;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.server.CacheServer#setTcpNoDelay(boolean)
	 */
	@Override
	public void setTcpNoDelay(final boolean tcpNoDelay) {
		this.tcpNoDelay = tcpNoDelay;
	}

	ClientSubscriptionConfig mockClientSubscriptionConfig() {
		return new MockClientSubscriptionConfig();
	}

	protected static class MockClientSubscriptionConfig implements ClientSubscriptionConfig {

		private int capacity;

		private String diskStoreName;
		private String evictionPolicy;
		private String overflowDirectory;

		@Override
		public int getCapacity() {
			return capacity;
		}

		@Override
		public void setCapacity(final int capacity) {
			this.capacity = capacity;
		}

		@Override
		public String getDiskStoreName() {
			return diskStoreName;
		}

		@Override
		public void setDiskStoreName(final String diskStoreName) {
			this.diskStoreName = diskStoreName;
		}

		@Override
		public String getEvictionPolicy() {
			return evictionPolicy;
		}

		@Override
		public void setEvictionPolicy(final String policy) {
			this.evictionPolicy = policy;
		}

		@Override
		public String getOverflowDirectory() {
			return overflowDirectory;
		}

		@Override
		public void setOverflowDirectory(final String overflowDirectory) {
			this.overflowDirectory = overflowDirectory;
		}
	}

}
