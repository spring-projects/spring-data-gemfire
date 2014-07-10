package org.springframework.data.gemfire.test;

import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import javax.naming.Context;

import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.gemstone.gemfire.CancelCriterion;
import com.gemstone.gemfire.LogWriter;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheTransactionManager;
import com.gemstone.gemfire.cache.CacheWriterException;
import com.gemstone.gemfire.cache.Declarable;
import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.DiskStoreFactory;
import com.gemstone.gemfire.cache.GatewayException;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionExistsException;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.RegionShortcut;
import com.gemstone.gemfire.cache.TimeoutException;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;
import com.gemstone.gemfire.cache.control.ResourceManager;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.IndexExistsException;
import com.gemstone.gemfire.cache.query.IndexInvalidException;
import com.gemstone.gemfire.cache.query.IndexNameConflictException;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.cache.query.RegionNotFoundException;
import com.gemstone.gemfire.cache.server.CacheServer;
import com.gemstone.gemfire.cache.snapshot.CacheSnapshotService;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.GatewayConflictResolver;
import com.gemstone.gemfire.cache.util.GatewayHub;
import com.gemstone.gemfire.cache.util.GatewayQueueAttributes;
import com.gemstone.gemfire.cache.wan.GatewayReceiver;
import com.gemstone.gemfire.cache.wan.GatewayReceiverFactory;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewaySenderFactory;
import com.gemstone.gemfire.distributed.DistributedMember;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.i18n.LogWriterI18n;
import com.gemstone.gemfire.pdx.PdxInstance;
import com.gemstone.gemfire.pdx.PdxInstanceFactory;
import com.gemstone.gemfire.pdx.PdxSerializer;

@SuppressWarnings({ "deprecation", "unused" })
public class StubCache implements Cache {

	protected static final String NOT_IMPLEMENTED = "Not Implemented!";

	private CacheTransactionManager cacheTransactionManager;

	private boolean closed;
	private boolean copyOnRead;
	private boolean pdxIgnoreUnreadFields;
	private boolean pdxPersistent;
	private boolean pdxReadSerialized;
	private boolean server;

	private int lockLease;
	private int lockTimeout;
	private int messageSyncInterval;
	private int searchTimeout;

	private Context jndiContext;

	private Declarable initializer;

	private DistributedSystem distributedSystem;

	private GatewayConflictResolver gatewayConflictResolver;

	private HashMap<String, Region> allRegions;

	private List<GatewayHub> gatewayHubs;

	private LogWriter logWriter;
	private LogWriter securityLogWriter;

	private PdxSerializer pdxSerializer;

	private Properties properties;

	private ResourceManager resourceManager;

	private Set<GatewayReceiver> gatewayReceivers;
	private Set<GatewaySender> gatewaySenders;

	private String pdxDiskStore;
	private String name;

	public StubCache(){
		this.allRegions = new HashMap<String,Region>();
	}
	
	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#createDiskStoreFactory()
	 */
	@Override
	public DiskStoreFactory createDiskStoreFactory() {
		return new StubDiskStore();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#findDiskStore(java.lang.String)
	 */
	@Override
	public DiskStore findDiskStore(String name) {
		return StubDiskStore.getDiskStore(name);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getCacheTransactionManager()
	 */
	@Override
	public CacheTransactionManager getCacheTransactionManager() {
		if (cacheTransactionManager == null) {
			cacheTransactionManager = new StubCacheTransactionMananger();
		}
		return cacheTransactionManager;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getCopyOnRead()
	 */
	@Override
	public boolean getCopyOnRead() {
		return this.copyOnRead;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getDistributedSystem()
	 */
	@Override
	public DistributedSystem getDistributedSystem() {
		if (distributedSystem == null) {
			distributedSystem = mockDistributedSystem();
		
		}
		return distributedSystem;
	}


	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getInitializer()
	 */
	@Override
	public Declarable getInitializer() {
		return this.initializer;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getInitializerProps()
	 */
	@Override
	public Properties getInitializerProps() {
		return this.properties;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getJNDIContext()
	 */
	@Override
	public Context getJNDIContext() {
		return this.jndiContext;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getLogger()
	 */
	@Override
	public LogWriter getLogger() {
		return this.logWriter;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getName()
	 */
	@Override
	public String getName() {
		return this.properties == null? null: (String)this.properties.get("name");
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getPdxDiskStore()
	 */
	@Override
	public String getPdxDiskStore() {
		return this.pdxDiskStore;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getPdxIgnoreUnreadFields()
	 */
	@Override
	public boolean getPdxIgnoreUnreadFields() {
		return this.pdxIgnoreUnreadFields;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getPdxPersistent()
	 */
	@Override
	public boolean getPdxPersistent() {
		return this.pdxPersistent;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getPdxReadSerialized()
	 */
	@Override
	public boolean getPdxReadSerialized() {
		return this.pdxReadSerialized;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getPdxSerializer()
	 */
	@Override
	public PdxSerializer getPdxSerializer() {
		return this.pdxSerializer;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getRegionAttributes(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <K, V> RegionAttributes<K, V> getRegionAttributes(String region) {
		return allRegions().get(region).getAttributes();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getResourceManager()
	 */
	@Override
	public ResourceManager getResourceManager() {
		return this.resourceManager;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#getSecurityLogger()
	 */
	@Override
	public LogWriter getSecurityLogger() {
		return this.securityLogWriter;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#listRegionAttributes()
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public <K, V> Map<String, RegionAttributes<K, V>> listRegionAttributes() {
		Map<String, RegionAttributes<K, V>> attributes = new HashMap<String, RegionAttributes<K, V>>();
		for (Entry<String, Region> entry: allRegions().entrySet()) {
			attributes.put(entry.getKey(), entry.getValue().getAttributes());
		}
		return attributes;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#loadCacheXml(java.io.InputStream)
	 */
	@Override
	public void loadCacheXml(InputStream is) throws TimeoutException, CacheWriterException, GatewayException, RegionExistsException {
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#setCopyOnRead(boolean)
	 */
	@Override
	public void setCopyOnRead(boolean arg0) {
		this.copyOnRead = arg0;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.GemFireCache#setRegionAttributes(java.lang.String, com.gemstone.gemfire.cache.RegionAttributes)
	 */
	@Override
	public <K, V> void setRegionAttributes(String region, RegionAttributes<K, V> attributes) {
		RegionFactory<K,V> rf = new MockRegionFactory<K, V>(this).createMockRegionFactory(attributes);
		rf.create(region);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.RegionService#close()
	 */
	@Override
	public void close() {
		this.closed = true;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.RegionService#createPdxEnum(java.lang.String, java.lang.String, int)
	 */
	@Override
	public PdxInstance createPdxEnum(String arg0, String arg1, int arg2) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.RegionService#createPdxInstanceFactory(java.lang.String)
	 */
	@Override
	public PdxInstanceFactory createPdxInstanceFactory(String arg0) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.RegionService#getCancelCriterion()
	 */
	@Override
	public CancelCriterion getCancelCriterion() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.RegionService#getQueryService()
	 */
	@Override
	public QueryService getQueryService() {
		QueryService qs = null;
		try {
			qs =  mockQueryService();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
		return qs;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.RegionService#getRegion(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <K, V> Region<K, V> getRegion(String name) {
		return allRegions().get(name);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.RegionService#isClosed()
	 */
	@Override
	public boolean isClosed() {
		return this.closed;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.RegionService#rootRegions()
	 */
	@Override
	public Set<Region<?, ?>> rootRegions() {
		Set<Region<?,?>> rootRegions = new HashSet<Region<?,?>>();
		for (String key: allRegions().keySet()) {
			if (!key.contains("/")) {
				rootRegions.add(allRegions().get(key));
			}
		}
		return rootRegions;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#addBridgeServer()
	 */
	@Override
	@Deprecated
	public com.gemstone.gemfire.cache.util.BridgeServer addBridgeServer() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#addCacheServer()
	 */
	@Override
	public CacheServer addCacheServer() {
		return mockCacheServer();
	}


	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#addGatewayHub(java.lang.String, int)
	 */
	@Override
	public GatewayHub addGatewayHub(String name, int port) {
		return mockGatewayHub();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#close(boolean)
	 */
	@Override
	@Deprecated
	public void close(boolean arg0) {
		this.closed = true;
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createAsyncEventQueueFactory()
	 */
	@Override
	public AsyncEventQueueFactory createAsyncEventQueueFactory() {
		return new StubAsyncEventQueueFactory();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createGatewayReceiverFactory()
	 */
	@Override
	public GatewayReceiverFactory createGatewayReceiverFactory() {
		return new StubGatewayReceiverFactory();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createGatewaySenderFactory()
	 */
	@Override
	public GatewaySenderFactory createGatewaySenderFactory() {
		return new StubGatewaySenderFactory();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createRegion(java.lang.String, com.gemstone.gemfire.cache.RegionAttributes)
	 */
	@Override
	@Deprecated
	public <K, V> Region<K, V> createRegion(String arg0, RegionAttributes<K, V> arg1) throws RegionExistsException, TimeoutException {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createRegionFactory()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <K, V> RegionFactory<K, V> createRegionFactory() {
		return new MockRegionFactory<K,V>(this).createRegionFactory();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createRegionFactory(com.gemstone.gemfire.cache.RegionAttributes)
	 */
	@Override
	public <K, V> RegionFactory<K, V> createRegionFactory(RegionAttributes<K, V> regionAttributes) {
		return new MockRegionFactory<K,V>(this).createMockRegionFactory(regionAttributes);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createRegionFactory(com.gemstone.gemfire.cache.RegionShortcut)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <K, V> RegionFactory<K, V> createRegionFactory(RegionShortcut shortcut) {
		return new MockRegionFactory<K,V>(this).createRegionFactory();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createRegionFactory(java.lang.String)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <K, V> RegionFactory<K, V> createRegionFactory(String regionAttributesId) {
		return new MockRegionFactory<K,V>(this).createRegionFactory();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#createVMRegion(java.lang.String, com.gemstone.gemfire.cache.RegionAttributes)
	 */
	@Override
	@Deprecated
	public <K, V> Region<K, V> createVMRegion(String arg0, RegionAttributes<K, V> arg1) throws RegionExistsException, TimeoutException {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getAdminMembers()
	 */
	@Override
	public Set<DistributedMember> getAdminMembers() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getAsyncEventQueue(java.lang.String)
	 */
	@Override
	public AsyncEventQueue getAsyncEventQueue(String name) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getAsyncEventQueues()
	 */
	@Override
	public Set<AsyncEventQueue> getAsyncEventQueues() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getBridgeServers()
	 */
	@Override
	@Deprecated
	public List<CacheServer> getBridgeServers() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getCacheServers()
	 */
	@Override
	public List<CacheServer> getCacheServers() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getGatewayConflictResolver()
	 */
	@Override
	public GatewayConflictResolver getGatewayConflictResolver() {
		return this.gatewayConflictResolver;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getGatewayHub()
	 */
	@Override
	@Deprecated
	public GatewayHub getGatewayHub() {
		return mockGatewayHub();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getGatewayHub(java.lang.String)
	 */
	@Override
	public GatewayHub getGatewayHub(String name) {
		return mockGatewayHub();
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getGatewayHubs()
	 */
	@Override
	public List<GatewayHub> getGatewayHubs() {
		return this.gatewayHubs;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getGatewayReceivers()
	 */
	@Override
	public Set<GatewayReceiver> getGatewayReceivers() {
		return this.gatewayReceivers;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getGatewaySender(java.lang.String)
	 */
	@Override
	public GatewaySender getGatewaySender(String name) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getGatewaySenders()
	 */
	@Override
	public Set<GatewaySender> getGatewaySenders() {
		return this.gatewaySenders;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getLockLease()
	 */
	@Override
	public int getLockLease() {
		return this.lockLease;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getLockTimeout()
	 */
	@Override
	public int getLockTimeout() {
		return this.lockTimeout;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getLoggerI18n()
	 */
	@Override
	@Deprecated
	public LogWriterI18n getLoggerI18n() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getMembers()
	 */
	@Override
	public Set<DistributedMember> getMembers() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getMembers(com.gemstone.gemfire.cache.Region)
	 */
	@SuppressWarnings({"rawtypes"})
	@Override
	public Set<DistributedMember> getMembers(Region arg0) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getMessageSyncInterval()
	 */
	@Override
	public int getMessageSyncInterval() {
		return this.messageSyncInterval;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getSearchTimeout()
	 */
	@Override
	public int getSearchTimeout() {
		return this.searchTimeout;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getSecurityLoggerI18n()
	 */
	@Override
	@Deprecated
	public LogWriterI18n getSecurityLoggerI18n() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#getSnapshotService()
	 */
	@Override
	public CacheSnapshotService getSnapshotService() {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#isServer()
	 */
	@Override
	public boolean isServer() {
		return this.server;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#readyForEvents()
	 */
	@Override
	@Deprecated
	public void readyForEvents() {
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#setGatewayConflictResolver(com.gemstone.gemfire.cache.util.GatewayConflictResolver)
	 */
	@Override
	public void setGatewayConflictResolver(GatewayConflictResolver arg0) {
		this.gatewayConflictResolver = arg0;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#setGatewayHub(java.lang.String, int)
	 */
	@Override
	@Deprecated
	public GatewayHub setGatewayHub(String arg0, int arg1) {
		throw new UnsupportedOperationException(NOT_IMPLEMENTED);
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#setIsServer(boolean)
	 */
	@Override
	public void setIsServer(boolean arg0) {
		this.server = arg0;
		
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#setLockLease(int)
	 */
	@Override
	public void setLockLease(int arg0) {
		this.lockLease = arg0;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#setLockTimeout(int)
	 */
	@Override
	public void setLockTimeout(int arg0) {
		this.lockTimeout = arg0;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#setMessageSyncInterval(int)
	 */
	@Override
	public void setMessageSyncInterval(int arg0) {
		this.messageSyncInterval = arg0;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.Cache#setSearchTimeout(int)
	 */
	@Override
	public void setSearchTimeout(int arg0) {
		this.searchTimeout = arg0;
	}

	DistributedSystem mockDistributedSystem() {
		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

		when(mockDistributedSystem.getName()).thenAnswer(new Answer<String>() {
			@Override
			public String answer(InvocationOnMock invocation) throws Throwable {
				return getName();
			}
		});

		when(mockDistributedSystem.getProperties()).thenReturn(this.properties);

		DistributedMember mockDistributedMember = mockDistributedMember();

		when(mockDistributedSystem.getDistributedMember()).thenReturn(mockDistributedMember);

		return mockDistributedSystem;
	}

	DistributedMember mockDistributedMember() {
		DistributedMember mockDistributedMember = mock(DistributedMember.class);
		when(mockDistributedMember.getHost()).thenReturn("mockDistributedMember.host");
		when(mockDistributedMember.getName()).thenReturn("mockDistributedMember");
		return mockDistributedMember;
	}

	CacheServer mockCacheServer() {
		return new StubCacheServer();
	}

	GatewayHub mockGatewayHub() {
		final Gateway gateway = mock(Gateway.class);

	    when(gateway.getQueueAttributes()).thenReturn(mock(GatewayQueueAttributes.class));

		GatewayHub gatewayHub = mock(GatewayHub.class);

		when(gatewayHub.addGateway(anyString(),anyInt())).thenAnswer(new Answer<Gateway>() {
			@Override
			public Gateway answer(InvocationOnMock invocation) throws Throwable {
				return gateway;
			}

		});

		return gatewayHub;
	}

	QueryService mockQueryService() throws RegionNotFoundException, IndexInvalidException, IndexNameConflictException, IndexExistsException, UnsupportedOperationException {
		QueryService queryService = mock(QueryService.class);

		when(queryService.getIndexes()).thenReturn(new ArrayList<Index>());

		when(queryService.createIndex(anyString(), anyString(),anyString())).thenAnswer(new Answer<Index>() {
			@Override
			public Index answer(InvocationOnMock invocation) throws Throwable {
				String indexName = (String) invocation.getArguments()[0];
				String indexedExpression = (String) invocation.getArguments()[1];
				String fromClause = (String) invocation.getArguments()[2];
				return mockIndex(indexName, com.gemstone.gemfire.cache.query.IndexType.FUNCTIONAL, indexedExpression,
					fromClause, null);
			}
		});

		when(queryService.createIndex(anyString(), anyString(),anyString(),anyString())).thenAnswer(new Answer<Index>() {
			@Override
			public Index answer(InvocationOnMock invocation) throws Throwable {
				String indexName = (String) invocation.getArguments()[0];
				String indexedExpression = (String) invocation.getArguments()[1];
				String fromClause = (String) invocation.getArguments()[2];
				String imports = (String) invocation.getArguments()[3];
				return mockIndex(indexName, com.gemstone.gemfire.cache.query.IndexType.FUNCTIONAL, indexedExpression,
					fromClause, imports);
			}
		});

		when(queryService.createKeyIndex(anyString(), anyString(),anyString())).thenAnswer(new Answer<Index>() {
			@Override
			public Index answer(InvocationOnMock invocation) throws Throwable {
				String indexName = (String) invocation.getArguments()[0];
				String indexedExpression = (String) invocation.getArguments()[1];
				String fromClause = (String) invocation.getArguments()[2];

				return mockIndex(indexName, com.gemstone.gemfire.cache.query.IndexType.PRIMARY_KEY, indexedExpression,
					fromClause, null);
			}
		});

		when(queryService.createHashIndex(anyString(), anyString(),anyString())).thenAnswer(new Answer<Index>() {
			@Override
			public Index answer(InvocationOnMock invocation) throws Throwable {
				String indexName = (String) invocation.getArguments()[0];
				String indexedExpression = (String) invocation.getArguments()[1];
				String fromClause = (String) invocation.getArguments()[2];

				return mockIndex(indexName, com.gemstone.gemfire.cache.query.IndexType.HASH, indexedExpression,
					fromClause, null);
			}
		});

		when(queryService.createHashIndex(anyString(), anyString(),anyString(),anyString())).thenAnswer(new Answer<Index>() {
			@Override
			public Index answer(InvocationOnMock invocation) throws Throwable {
				String indexName = (String) invocation.getArguments()[0];
				String indexedExpression = (String) invocation.getArguments()[1];
				String fromClause = (String) invocation.getArguments()[2];
				String imports = (String) invocation.getArguments()[3];

				return mockIndex(indexName, com.gemstone.gemfire.cache.query.IndexType.HASH, indexedExpression,
					fromClause, imports);
			}
		});

		return queryService;
	}

	@SuppressWarnings({ "rawtypes", "unchecked", "unused" })
	Index mockIndex(String indexName, com.gemstone.gemfire.cache.query.IndexType indexType, String indexedExpression,
			String fromClause, String imports){
		Index idx = mock(Index.class);
		when(idx.getFromClause()).thenReturn(fromClause);
		when(idx.getIndexedExpression()).thenReturn(indexedExpression);
		when(idx.getName()).thenReturn(indexName);
		when(idx.getType()).thenReturn(indexType);
		
		
		if (fromClause != null && fromClause.length() >= 2) {
			Region region = mock(Region.class);
			String name = fromClause.substring(1).split(" ")[0];
			when(region.getName()).thenReturn(name);
			when(idx.getRegion()).thenReturn(region);
		}
		return idx;
	}

	@SuppressWarnings("rawtypes")
	public Map<String,Region> allRegions() {
		return this.allRegions;
	}

	public void setProperties(Properties gemfireProperties) {
		this.properties = gemfireProperties;
	}

	@Override
	public boolean isReconnecting() {
		return false;
	}

	@Override
	public Cache getReconnectedCache() {
		return this;
	}

	@Override
	public void stopReconnecting() {
	}

	@Override
	public boolean waitUntilReconnected(final long l, final TimeUnit timeUnit) throws InterruptedException {
		return false;
	}

}
