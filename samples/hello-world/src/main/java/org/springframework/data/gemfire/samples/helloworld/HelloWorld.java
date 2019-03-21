/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.samples.helloworld;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.annotation.Resource;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Component;

import org.apache.geode.cache.Region;

/**
 * Main bean for interacting with the cache from the console.
 *
 * @author Costin Leau
 */
@Component
public class HelloWorld {

	private static final Log log = LogFactory.getLog(HelloWorld.class);

	// inject the region
	@Resource(name = "myWorld")
	private Region<String, String> region;

	@Resource
	private CommandProcessor processor;

	@PostConstruct
	void start() {
		log.info("Member " + region.getCache().getDistributedSystem().getDistributedMember().getId()
				+ " connecting to region [" + region.getName() + "]");
		processor.start();
	}

	@PreDestroy
	void stop() throws Exception {
		log.info("Member " + region.getCache().getDistributedSystem().getDistributedMember().getId()
				+ " disconnecting from region [" + region.getName() + "]");
		processor.stop();
	}

	public void greetWorld() {
		try {
			processor.awaitCommands();
		} catch (Exception ex) {
			throw new IllegalStateException("Cannot greet world", ex);
		}
	}
}
